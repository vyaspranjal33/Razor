// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Text;
using Xunit;

namespace Microsoft.CodeAnalysis.Razor.ProjectSystem
{
    public class DefaultDocumentSnapshotTest : WorkspaceTestBase
    {
        public DefaultDocumentSnapshotTest()
        {
            var projectState = ProjectState.Create(Workspace.Services, TestProjectData.SomeProject);
            var project = new DefaultProjectSnapshot(projectState);
            HostDocument = new HostDocument(TestProjectData.SomeProjectFile1.FilePath, TestProjectData.SomeProjectFile1.TargetPath);
            SourceText = SourceText.From("<p>Hello World</p>");
            Version = VersionStamp.Create();
            var textAndVersion = TextAndVersion.Create(SourceText, Version);
            var documentState = DocumentState.Create(Workspace.Services, HostDocument, () => Task.FromResult(textAndVersion));
            Document = new DefaultDocumentSnapshot(project, documentState);
        }

        private SourceText SourceText { get; }

        private VersionStamp Version { get; }

        private HostDocument HostDocument { get; }

        private DefaultDocumentSnapshot Document { get; }

        private TestGeneratedOutputPublisher Publisher { get; } = new TestGeneratedOutputPublisher();

        protected override void ConfigureLanguageServices(List<ILanguageService> services)
        {
            services.Add(new TestTagHelperResolver());
        }

        protected override void ConfigureWorkspaceServices(List<IWorkspaceService> services)
        {
            services.RemoveAll(s => s is GeneratedOutputPublisher);
            services.Add(Publisher);
        }

        [Fact]
        public async Task GetGeneratedOutputAsync_PublishesOutput()
        {
            // Act
            await Document.GetGeneratedOutputAsync();

            // Assert
            var result = Assert.Single(Publisher.Output);
            Assert.Same(result.document, Document);
        }

        // This tests that the 'Version' of the generated output is computed based on
        // the version of the inputs. We can't create a timestamp when we generate the code
        // because operations might not happen in order.
        [Fact]
        public async Task GetGeneratedOutputAsync_SetsOutputWhenDocumentIsNewer()
        {
            // Arrange
            var newSourceText = SourceText.From("NEW!");
            var newDocumentState = Document.State.WithText(newSourceText, Version.GetNewerVersion());
            var newDocument = new DefaultDocumentSnapshot(Document.ProjectInternal, newDocumentState);

            // Force the output to be the new output
            await Document.GetGeneratedOutputAsync();

            // Act
            await newDocument.GetGeneratedOutputAsync();

            // Assert
            Assert.Collection(
                Publisher.Output,
                (o) => Assert.Same(Document, o.document),
                (o) => Assert.Same(newDocument, o.document));
        }

        [Fact]
        public async Task GetGeneratedOutputAsync_OnlySetsOutputIfDocumentNewer()
        {
            // Arrange
            var newSourceText = SourceText.From("NEW!");
            var newDocumentState = Document.State.WithText(newSourceText, Version.GetNewerVersion());
            var newDocument = new DefaultDocumentSnapshot(Document.ProjectInternal, newDocumentState);

            // Trigger code generation based on the new version - it's version should be
            // based on the text.
            var newerVersion = await newDocument.GetGeneratedOutputVersionAsync();
            Assert.Equal(newerVersion, await newDocument.GetTextVersionAsync());

            // Act
            var olderVersion = await Document.GetGeneratedOutputVersionAsync();

            // Assert
            Assert.Collection(
                Publisher.Output,
                (o) => Assert.Same(newDocument, o.document),
                (o) => Assert.Same(Document, o.document));
        }
    }
}
