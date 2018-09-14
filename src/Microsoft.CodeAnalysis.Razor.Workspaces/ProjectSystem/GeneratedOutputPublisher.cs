// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Microsoft.AspNetCore.Razor.Language;
using Microsoft.CodeAnalysis.Host;

namespace Microsoft.CodeAnalysis.Razor.ProjectSystem
{
    /// <summary>
    /// Publishes the results of code generation ***BEFORE** 
    /// <see cref="DocumentSnapshot.GetGeneratedOutputAsync"/> completes. This allows pushing the generated
    /// code to an external sink so that it can be accessed across a boundary once the task completes.
    /// 
    /// DO NOT use this to subscribe to the notified when code generation completes as a generic
    /// notification service. Use <see cref="GeneratedOutputChangeTrigger"/>.
    /// </summary>
    internal abstract class GeneratedOutputPublisher : IWorkspaceService
    {
        public abstract Task PublishGeneratedOutputAsync(DefaultDocumentSnapshot document, RazorCodeDocument output, VersionStamp version);
    }
}
