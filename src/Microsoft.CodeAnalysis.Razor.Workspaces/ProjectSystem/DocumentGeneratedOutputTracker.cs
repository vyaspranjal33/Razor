// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Razor.Language;
using Microsoft.CodeAnalysis.Text;
using Microsoft.Extensions.Internal;

namespace Microsoft.CodeAnalysis.Razor.ProjectSystem
{
    internal class DocumentGeneratedOutputTracker
    {
        private readonly object _lock;

        // The base version is the version stamp of the last usable generated
        // output. This allows us have a version that represents the last time the
        // generated output was *reset* due to a configuration change.
        private readonly VersionStamp _baseVersion;

        private DocumentGeneratedOutputTracker _older;
        private Task<(RazorCodeDocument output, VersionStamp version)> _task;
        
        private IReadOnlyList<TagHelperDescriptor> _tagHelpers;
        private IReadOnlyList<ImportItem> _imports;

        public DocumentGeneratedOutputTracker(DocumentGeneratedOutputTracker older)
        {
            _older = older;
            _baseVersion = older?._baseVersion ?? VersionStamp.Create();
            _lock = new object();
        }

        public bool IsResultAvailable => _task?.IsCompleted == true;

        public DocumentGeneratedOutputTracker Older => _older;

        public Task<(RazorCodeDocument output, VersionStamp version)> GetGeneratedOutput(DefaultProjectSnapshot project, DefaultDocumentSnapshot document)
        {
            if (project == null)
            {
                throw new ArgumentNullException(nameof(project));
            }

            if (document == null)
            {
                throw new ArgumentNullException(nameof(document));
            }

            if (_task == null)
            {
                lock (_lock)
                {
                    if (_task == null)
                    {
                        _task = GetGeneratedOutputCore(project, document);
                    }
                }
            }

            return _task;
        }

        public DocumentGeneratedOutputTracker Fork()
        {
            return new DocumentGeneratedOutputTracker(this);
        }

        private async Task<(RazorCodeDocument output, VersionStamp version)> GetGeneratedOutputCore(DefaultProjectSnapshot project, DefaultDocumentSnapshot document)
        {
            var imports = await GetImportsAsync(project, document).ConfigureAwait(false);
            var tagHelpers = await project.GetTagHelpersAsync().ConfigureAwait(false);
            if (_older?._tagHelpers != null)
            {
                var tagHelperDifference = new HashSet<TagHelperDescriptor>(TagHelperDescriptorComparer.Default);
                tagHelperDifference.UnionWith(_older._tagHelpers);
                tagHelperDifference.SymmetricExceptWith(tagHelpers);

                var importDifference = new HashSet<ImportItem>();
                importDifference.UnionWith(_older._imports);
                importDifference.SymmetricExceptWith(imports);

                if (tagHelperDifference.Count == 0 && importDifference.Count == 0)
                {
                    // We can use the cached result.
                    var result = _older._task.Result;

                    // Cache the tag helpers and imports so the next version can use them
                    _tagHelpers = tagHelpers;
                    _imports = imports;

                    // Drop reference so it can be GC'ed
                    _older = null;

                    return result;
                }
            }

            // Cache the tag helpers and imports so the next version can use them
            _tagHelpers = tagHelpers;
            _imports = imports;

            // Drop reference so it can be GC'ed
            _older = null;

            // Track the version of all of the inputs, so we can use it as the version of the generated code.
            var version = _baseVersion;
            var importSources = new List<RazorSourceDocument>();
            foreach (var import in imports)
            {
                version = version.GetNewerVersion(import.Version);

                var sourceDocument = await GetRazorSourceDocumentAsync(import.Import);
                importSources.Add(sourceDocument);
            }

            var tagHelperVersion = await project.WorkspaceProject.GetDependentVersionAsync().ConfigureAwait(false);
            version = version.GetNewerVersion(tagHelperVersion);

            var documentSource = await GetRazorSourceDocumentAsync(document).ConfigureAwait(false);
            var documentVersion = await document.GetTextVersionAsync().ConfigureAwait(false);
            version = version.GetNewerVersion(documentVersion);

            var projectEngine = project.GetProjectEngine();

            var codeDocument = projectEngine.ProcessDesignTime(documentSource, importSources, tagHelpers);

            // The publisher is called **BEFORE** the output is available (before the task completes).
            // This is for scenarios like omnisharp where we want to block completion of the codegen
            // until we have pushed the generated output somewhere else.
            var publisher = project.State.Services.GetRequiredService<GeneratedOutputPublisher>();
            await publisher.PublishGeneratedOutputAsync(document, codeDocument, version);

            return (codeDocument, version);
        }

        private async Task<RazorSourceDocument> GetRazorSourceDocumentAsync(DocumentSnapshot document)
        {
            var sourceText = await document.GetTextAsync();
            return sourceText.GetRazorSourceDocument(document.FilePath);
        }

        private async Task<IReadOnlyList<ImportItem>> GetImportsAsync(DefaultProjectSnapshot project, DefaultDocumentSnapshot document)
        {
            var imports = new List<ImportItem>();
            foreach (var snapshot in document.GetImports())
            {
                var versionStamp = await snapshot.GetTextVersionAsync();
                imports.Add(new ImportItem(snapshot.FilePath, versionStamp, snapshot));
            }

            return imports;
        }

        private readonly struct ImportItem : IEquatable<ImportItem>
        {
            public ImportItem(string filePath, VersionStamp versionStamp, DocumentSnapshot import)
            {
                FilePath = filePath;
                Version = versionStamp;
                Import = import;
            }

            public string FilePath { get; }

            public VersionStamp Version { get; }

            public DocumentSnapshot Import { get; }

            public bool Equals(ImportItem other)
            {
                return
                    FilePathComparer.Instance.Equals(FilePath, other.FilePath) &&
                    Version == other.Version;
            }

            public override bool Equals(object obj)
            {
                return obj is ImportItem item ? Equals(item) : false;
            }

            public override int GetHashCode()
            {
                var hash = new HashCodeCombiner();
                hash.Add(FilePath, FilePathComparer.Instance);
                hash.Add(Version);
                return hash;
            }
        }
    }
}
