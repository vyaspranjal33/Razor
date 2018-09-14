// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Classification;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Razor.ProjectSystem;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Razor
{
    internal class RazorDocumentExerptService : IDocumentExcerptService
    {
        private readonly DocumentSnapshot _document;
        private readonly ISpanMappingService _mapper;

        public RazorDocumentExerptService(DocumentSnapshot document, ISpanMappingService mapper)
        {
            if (mapper == null)
            {
                throw new ArgumentNullException(nameof(mapper));
            }

            _document = document;
            _mapper = mapper;
        }

        public async Task<ExcerptResult?> TryExcerptAsync(
            Document document,
            TextSpan span,
            ExcerptMode mode,
            CancellationToken cancellationToken)
        {
            if (_document == null)
            {
                return null;
            }

            var mapped = await _mapper.MapSpansAsync(document, new[] { span }, cancellationToken).ConfigureAwait(false);
            if (mapped.Length == 0 || mapped[0].Equals(default(MappedSpanResult)))
            {
                return null;
            }

            var project = _document.Project;
            var primaryDocument = project.GetDocument(mapped[0].FilePath);
            if (primaryDocument == null)
            {
                return null;
            }

            var primaryText = await primaryDocument.GetTextAsync().ConfigureAwait(false);
            var primarySpan = mapped[0].Span;

            var secondaryDocument = document;
            var secondarySpan = span;

            var (exerptText, exerptSpan) = GetExerpt(primaryText, primarySpan);

            var classifiedSpans = await ClassifyPreviewAsync(exerptText, exerptSpan, secondaryDocument, secondarySpan, cancellationToken).ConfigureAwait(false);

            return new ExcerptResult(exerptText, exerptSpan, classifiedSpans.ToImmutable(), document, span);
        }

        public (SourceText exerptText, TextSpan exerptSpan) GetExerpt(SourceText text, TextSpan span)
        {
            // Compute a subtext based that includes whole lines of the text enclosed in the mapped span.
            var exerptStart = text.Lines.GetLineFromPosition(span.Start).Start;
            var exerptEnd = text.Lines.GetLineFromPosition(span.End).End;

            var exerptText = text.GetSubText(new TextSpan(exerptStart, exerptEnd - exerptStart));

            // Convert the mapped span to a new span relative to the preview text
            var exerptSpan = new TextSpan(span.Start - exerptStart, span.Length);

            return (exerptText, exerptSpan);
        }

        public async Task<ImmutableArray<ClassifiedSpan>.Builder> ClassifyPreviewAsync(
            SourceText exerptText,
            TextSpan exerptSpan,
            Document secondaryDocument,
            TextSpan secondarySpan,
            CancellationToken cancellationToken)
        {
            var builder = ImmutableArray.CreateBuilder<ClassifiedSpan>();
            if (exerptSpan.Start > 0)
            {
                // There is some leading text that isn't C#
                builder.Add(new ClassifiedSpan(ClassificationTypeNames.Text, new TextSpan(0, exerptSpan.Length)));
            }

            var classifiedSecondarySpans = await Classifier.GetClassifiedSpansAsync(secondaryDocument, secondarySpan, cancellationToken);

            var offset = secondarySpan.Start - exerptSpan.Start;
            foreach (var classifiedSecondarySpan in classifiedSecondarySpans)
            {
                // Offset each span to match the excerpt text

                var span = new TextSpan(classifiedSecondarySpan.TextSpan.Start - offset, classifiedSecondarySpan.TextSpan.Length);
                builder.Add(new ClassifiedSpan(classifiedSecondarySpan.ClassificationType, span));
            }

            if (exerptText.Length - exerptSpan.End > 0)
            {
                // There is some trailing text that isn't C#
                builder.Add(new ClassifiedSpan(ClassificationTypeNames.Text, new TextSpan(exerptSpan.End, exerptText.Length - exerptSpan.End)));
            }

            return builder;
        }
    }
}