module ProtocolMessage = struct

  type t =
    | Request
    | Response
    | Event

  let enc_t : t Data_encoding.t =
    Data_encoding.conv
      (function | Request -> "request" | Response -> "response" | Event -> "event" )
      (function | "request" -> Request | "response" -> Response | "event" -> Event | _ -> failwith "Unknown message type")
      Data_encoding.string

  type cls_t = < seq:int; type_:t >

  class cls (seq:int) (type_:t) = object
    method seq = seq
    method type_ = type_
  end

end

module type ENC_0 = sig
  type t
  val enc : t Data_encoding.t
end

module type ENC_1 = sig
  type 'a t
  val enc : 'a Data_encoding.t -> 'a t Data_encoding.t
end


module Message = struct

    (* TODO not sure whats going on with the variables field
       "Message": {
     *   "type": "object",
     *   "description": "A structured message object. Used to return errors from requests.",
     *   "properties": {
     *     "id": {
     *       "type": "integer",
     *       "description": "Unique identifier for the message."
     *     },
     *     "format": {
     *       "type": "string",
     *       "description": "A format string for the message. Embedded variables have the form '{name}'.\nIf variable name starts with an underscore character, the variable does not contain user data (PII) and can be safely used for telemetry purposes."
     *     },
     *     "variables": {
     *       "type": "object",
     *       "description": "An object used as a dictionary for looking up the variables in the format string.",
     *       "additionalProperties": {
     *         "type": "string",
     *         "description": "Values must be strings."
     *       }
     *     },
     *     "sendTelemetry": {
     *       "type": "boolean",
     *       "description": "If true send to telemetry."
     *     },
     *     "showUser": {
     *       "type": "boolean",
     *       "description": "If true show user."
     *     },
     *     "url": {
     *       "type": "string",
     *       "description": "An optional url where additional information about this message can be found."
     *     },
     *     "urlLabel": {
     *       "type": "string",
     *       "description": "An optional label that is presented to the user as the UI for opening the url."
     *     }
     *   },
     *   "required": ["id", "format"]
     * }, *)


  type t = {
    id: int;
    format: string;
    variables: (string * string) list option;
    sendTelemetry: bool option;
    showUser: bool option;
    url: string option;
    urlLabel: string option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {id; format; variables; sendTelemetry; showUser; url; urlLabel} -> (id, format, variables, sendTelemetry, showUser, url, urlLabel))
      (fun (id, format, variables, sendTelemetry, showUser, url, urlLabel) -> {id; format; variables; sendTelemetry; showUser; url; urlLabel})
      (obj7
         (req "id" int31)
         (req "format" string)
         (opt "variables" @@ list @@ tup2 string string)
         (opt "sendTelemetry" bool)
         (opt "showUser" bool)
         (opt "url" string)
         (opt "urlLabel" string))

end


module ChecksumAlgorithm = struct

  type t =
    | MD5
    | SHA1
    | SHA256
    | Timestamp

  let enc =
    let open Data_encoding in
    conv
      (function | MD5 -> "MD5" | SHA1 -> "SHA1" | SHA256 -> "SHA256" | Timestamp -> "timestamp")
      (function | "MD5" -> MD5 | "SHA1" -> SHA1 | "SHA256" -> SHA256 | "timestamp" -> Timestamp | _ -> failwith "Unknown Checksum")
      string

end

module Checksum = struct

  type t = {
    algorithm: ChecksumAlgorithm.t;
    checksum: string;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {algorithm; checksum} -> (algorithm, checksum))
      (fun (algorithm, checksum) -> {algorithm; checksum})
      (obj2
         (req "algorithm" ChecksumAlgorithm.enc)
         (req "checksum" string))

end


module Source = struct

  type hint =
    | Normal
    | Emphasize
    | Deemphasize

  let hint_enc =
    let open Data_encoding in
    conv
      (function | Normal -> "normal" | Emphasize -> "emphasize" | Deemphasize -> "deemphasize")
      (function | "normal" -> Normal | "emphasize" -> Emphasize | "deemphasize" -> Deemphasize | _ -> failwith "Unknown hint")
      string

  type 'json t = {
    name: string option;
    path: string option;
    sourceReference: int option;
    presentationHint: hint option;
    origin: string option;
    sources: 'json t list option;
    adapterData: 'json option;
    checksums: Checksum.t list option;
  }

  let enc json_enc =
    let open Data_encoding in
    mu "t" (fun e ->
        conv
          (fun {
             name;
             path;
             sourceReference;
             presentationHint;
             origin;
             sources;
             adapterData;
             checksums;
           } -> (
               name,
               path,
               sourceReference,
               presentationHint,
               origin,
               sources,
               adapterData,
               checksums
             ))
          (fun (
             name,
             path,
             sourceReference,
             presentationHint,
             origin,
             sources,
             adapterData,
             checksums
           ) -> {
               name;
               path;
               sourceReference;
               presentationHint;
               origin;
               sources;
               adapterData;
               checksums;
             })
          (obj8
             (opt "name" string)
             (opt "path" string)
             (opt "sourceReference" int31)
             (opt "presentationHint" hint_enc)
             (opt "origin" string)
             (opt "sources" (list e))
             (opt "adapterData" json_enc)
             (opt "checksums" @@ list Checksum.enc)
          )
      )
end


module Reason = struct

  type t =
    | New
    | Changed
    | Removed

  let enc =
    let open Data_encoding in
    conv
      (function
        | New -> "new"
        | Changed -> "changed"
        | Removed -> "removed"
      )
      (function
        | "new" -> New
        | "changed" -> Changed
        | "removed" -> Removed
        | _ -> failwith "Unknown reason"
      )
      string
end


module Breakpoint = struct

  type 'json t = {
    id: int option;
    verified: bool;
    message: string option;
    source: 'json Source.t option;
    line: int option;
    column: int option;
    endLine: int option;
    endColumn: int option;
    instructionReference: string option;
    offset: int option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {
         id;
         verified;
         message;
         source;
         line;
         column;
         endLine;
         endColumn;
         instructionReference;
         offset;
       } -> (
           id,
           verified,
           message,
           source,
           line,
           column,
           endLine,
           endColumn,
           instructionReference,
           offset
         )
      )
      (fun (
         id,
         verified,
         message,
         source,
         line,
         column,
         endLine,
         endColumn,
         instructionReference,
         offset
       ) -> {
           id;
           verified;
           message;
           source;
           line;
           column;
           endLine;
           endColumn;
           instructionReference;
           offset;
         }
      )
      (obj10
         (opt "id" int31)
         (req "verified" bool)
         (opt "message" string)
         (opt "source" @@ Source.enc json)
         (opt "line" int31)
         (opt "column" int31)
         (opt "endLine" int31)
         (opt "endColumn" int31)
         (opt "instructionReference" string)
         (opt "offset" int31)
      )

end



module Module_ = struct

  type id =
    | I of int
    | S of string

  let enc_id =
    let open Data_encoding in
    conv
      (function | I i -> string_of_int i | S s -> s)
      (fun id ->
         match int_of_string_opt id with
         | Some i -> I i
         | None -> S id)
      string

  type t = {
    id: id;
    name: string;
    path: string option;
    isOptimized: bool option;
    isUserCode: bool option;
    version: string option;
    symbolStatus: string option;
    symbolFilePath: string option;
    dateTimeStamp: string option;
    addressRange: string option;
  }


  let enc =
    let open Data_encoding in
    conv
      (fun {
         id;
         name;
         path;
         isOptimized;
         isUserCode;
         version;
         symbolStatus;
         symbolFilePath;
         dateTimeStamp;
         addressRange;
       } -> (
           id,
           name,
           path,
           isOptimized,
           isUserCode,
           version,
           symbolStatus,
           symbolFilePath,
           dateTimeStamp,
           addressRange
         )
      )
      (fun (
         id,
         name,
         path,
         isOptimized,
         isUserCode,
         version,
         symbolStatus,
         symbolFilePath,
         dateTimeStamp,
         addressRange
       ) -> {
           id;
           name;
           path;
           isOptimized;
           isUserCode;
           version;
           symbolStatus;
           symbolFilePath;
           dateTimeStamp;
           addressRange;
         }
      )
      (obj10
         (req "id" enc_id)
         (req "name" string)
         (opt "path" string)
         (opt "isOptimized" bool)
         (opt "isUserCode" bool)
         (opt "version" string)
         (opt "symbolStatus" string)
         (opt "symbolFilePath" string)
         (opt "dateTimeStamp" string)
         (opt "addressRange" string)
      )


end


module ExceptionBreakpointsFilter = struct

  type t = {
    filter: string;
    label: string;
    description: string option;
    default: bool option;
    supportsCondition: bool option;
    conditionDescription: string option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {
         filter;
         label;
         description;
         default;
         supportsCondition;
         conditionDescription;
       } -> (
           filter,
           label,
           description,
           default,
           supportsCondition,
           conditionDescription
         ))
      (fun (
         filter,
         label,
         description,
         default,
         supportsCondition,
         conditionDescription
       ) -> {
           filter;
           label;
           description;
           default;
           supportsCondition;
           conditionDescription;
         })
      (obj6
         (req "filter" string)
         (req "label" string)
         (opt "description" string)
         (opt "default" bool)
         (opt "supportsCondition" bool)
         (opt "conditionDescription" string)
      )

end

module ColumnDescriptor = struct

  type column_type =
    | String
    | Number
    | Boolean
    | UnixTimestampUTC

  let column_type_enc =
    let open Data_encoding in
    conv
      (function
        | String -> "string"
        | Number -> "number"
        | Boolean -> "boolean"
        | UnixTimestampUTC -> "unixTimestampUTC"
      )
      (function
        | "string" -> String
        | "number" -> Number
        | "boolean" -> Boolean
        | "unixTimestampUTC" -> UnixTimestampUTC
        | _ -> failwith "Unknown column type"
      )
      string

  type t = {
    attributeName: string;
    label: string;
    format: string option;
    type_: column_type option;
    width: int option
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {
         attributeName;
         label;
         format;
         type_;
         width;
       } -> (
           attributeName,
           label,
           format,
           type_,
           width
         ))
      (fun (
           attributeName,
           label,
           format,
           type_,
           width
         ) -> {
         attributeName;
         label;
         format;
         type_;
         width;
       })
      (obj5
         (req "attributeName" string)
         (req "label" string)
         (opt "format" string)
         (opt "type" column_type_enc)
         (opt "width" int31)
      )
end


module Capabilities = struct

  (* NOTE encoders only support up to 10 fields for an object
     so will have to group these by 10s *)
  type t0 = {
    supportsConfigurationDoneRequest: bool option;
    supportsFunctionBreakpoints: bool option;
    supportsConditionalBreakpoints: bool option;
    supportsHitConditionalBreakpoints: bool option;
    supportsEvaluateForHovers: bool option;
    exceptionBreakpointFilters: ExceptionBreakpointsFilter.t list option;
    supportsStepBack: bool option;
    supportsSetVariable: bool option;
    supportsRestartFrame: bool option;
    supportsGotoTargetsRequest: bool option;
  }

  type t1 = {
    supportsStepInTargetsRequest: bool option;
    supportsCompletionsRequest: bool option;
    completionTriggerCharacters: string list option;
    supportsModulesRequest: bool option;
    additionalModuleColumns: ColumnDescriptor.t list option;
    supportedChecksumAlgorithms: ChecksumAlgorithm.t list option;
    supportsRestartRequest: bool option;
    supportsExceptionOptions: bool option;
    supportsValueFormattingOptions: bool option;
    supportsExceptionInfoRequest: bool option;
  }

  type t2 = {
    supportTerminateDebuggee: bool option;
    supportSuspendDebuggee: bool option;
    supportsDelayedStackTraceLoading: bool option;
    supportsLoadedSourcesRequest: bool option;
    supportsLogPoints: bool option;
    supportsTerminateThreadsRequest: bool option;
    supportsSetExpression: bool option;
    supportsTerminateRequest: bool option;
    supportsDataBreakpoints: bool option;
    supportsReadMemoryRequest: bool option;
  }

  type t3 = {
    supportsWriteMemoryRequest: bool option;
    supportsDisassembleRequest: bool option;
    supportsCancelRequest: bool option;
    supportsBreakpointLocationsRequest: bool option;
    supportsClipboardContext: bool option;
    supportsSteppingGranularity: bool option;
    supportsInstructionBreakpoints: bool option;
    supportsExceptionFilterOptions: bool option;
    supportsSingleThreadExecutionRequests: bool option;
  }

  let enc_t0 =
    let open Data_encoding in
    conv
      (fun {
         supportsConfigurationDoneRequest;
         supportsFunctionBreakpoints;
         supportsConditionalBreakpoints;
         supportsHitConditionalBreakpoints;
         supportsEvaluateForHovers;
         exceptionBreakpointFilters;
         supportsStepBack;
         supportsSetVariable;
         supportsRestartFrame;
         supportsGotoTargetsRequest;
       } -> (
           supportsConfigurationDoneRequest,
           supportsFunctionBreakpoints,
           supportsConditionalBreakpoints,
           supportsHitConditionalBreakpoints,
           supportsEvaluateForHovers,
           exceptionBreakpointFilters,
           supportsStepBack,
           supportsSetVariable,
           supportsRestartFrame,
           supportsGotoTargetsRequest
         )
      )
      (fun (
         supportsConfigurationDoneRequest,
         supportsFunctionBreakpoints,
         supportsConditionalBreakpoints,
         supportsHitConditionalBreakpoints,
         supportsEvaluateForHovers,
         exceptionBreakpointFilters,
         supportsStepBack,
         supportsSetVariable,
         supportsRestartFrame,
         supportsGotoTargetsRequest
       ) -> {
           supportsConfigurationDoneRequest;
           supportsFunctionBreakpoints;
           supportsConditionalBreakpoints;
           supportsHitConditionalBreakpoints;
           supportsEvaluateForHovers;
           exceptionBreakpointFilters;
           supportsStepBack;
           supportsSetVariable;
           supportsRestartFrame;
           supportsGotoTargetsRequest;
         }
      )
      (obj10
         (opt "supportsConfigurationDoneRequest" bool)
         (opt "supportsFunctionBreakpoints" bool)
         (opt "supportsConditionalBreakpoints" bool)
         (opt "supportsHitConditionalBreakpoints" bool)
         (opt "supportsEvaluateForHovers" bool)
         (opt "exceptionBreakpointFilters" @@ list ExceptionBreakpointsFilter.enc)
         (opt "supportsStepBack" bool)
         (opt "supportsSetVariable" bool)
         (opt "supportsRestartFrame" bool)
         (opt "supportsGotoTargetsRequest" bool)
      )

  let enc_t1 =
    let open Data_encoding in
    conv
      (fun {
         supportsStepInTargetsRequest;
         supportsCompletionsRequest;
         completionTriggerCharacters;
         supportsModulesRequest;
         additionalModuleColumns;
         supportedChecksumAlgorithms;
         supportsRestartRequest;
         supportsExceptionOptions;
         supportsValueFormattingOptions;
         supportsExceptionInfoRequest;
       } -> (
           supportsStepInTargetsRequest,
           supportsCompletionsRequest,
           completionTriggerCharacters,
           supportsModulesRequest,
           additionalModuleColumns,
           supportedChecksumAlgorithms,
           supportsRestartRequest,
           supportsExceptionOptions,
           supportsValueFormattingOptions,
           supportsExceptionInfoRequest
         )
      )
      (fun (
         supportsStepInTargetsRequest,
         supportsCompletionsRequest,
         completionTriggerCharacters,
         supportsModulesRequest,
         additionalModuleColumns,
         supportedChecksumAlgorithms,
         supportsRestartRequest,
         supportsExceptionOptions,
         supportsValueFormattingOptions,
         supportsExceptionInfoRequest
       ) -> {
           supportsStepInTargetsRequest;
           supportsCompletionsRequest;
           completionTriggerCharacters;
           supportsModulesRequest;
           additionalModuleColumns;
           supportedChecksumAlgorithms;
           supportsRestartRequest;
           supportsExceptionOptions;
           supportsValueFormattingOptions;
           supportsExceptionInfoRequest;
         }
      )
      (obj10
         (opt "supportsStepInTargetsRequest" bool)
         (opt "supportsCompletionsRequest" bool)
         (opt "completionTriggerCharacters" @@ list string)
         (opt "supportsModulesRequest" bool)
         (opt "additionalModuleColumns" @@ list ColumnDescriptor.enc)
         (opt "supportedChecksumAlgorithms" @@ list ChecksumAlgorithm.enc)
         (opt "supportsRestartRequest" bool)
         (opt "supportsExceptionOptions" bool)
         (opt "supportsValueFormattingOptions" bool)
         (opt "supportsExceptionInfoRequest" bool)
      )

  let enc_t2 =
    let open Data_encoding in
    conv
      (fun {
         supportTerminateDebuggee;
         supportSuspendDebuggee;
         supportsDelayedStackTraceLoading;
         supportsLoadedSourcesRequest;
         supportsLogPoints;
         supportsTerminateThreadsRequest;
         supportsSetExpression;
         supportsTerminateRequest;
         supportsDataBreakpoints;
         supportsReadMemoryRequest;
       } -> (
           supportTerminateDebuggee,
           supportSuspendDebuggee,
           supportsDelayedStackTraceLoading,
           supportsLoadedSourcesRequest,
           supportsLogPoints,
           supportsTerminateThreadsRequest,
           supportsSetExpression,
           supportsTerminateRequest,
           supportsDataBreakpoints,
           supportsReadMemoryRequest
         )
      )
      (fun (
         supportTerminateDebuggee,
         supportSuspendDebuggee,
         supportsDelayedStackTraceLoading,
         supportsLoadedSourcesRequest,
         supportsLogPoints,
         supportsTerminateThreadsRequest,
         supportsSetExpression,
         supportsTerminateRequest,
         supportsDataBreakpoints,
         supportsReadMemoryRequest
       ) -> {
           supportTerminateDebuggee;
           supportSuspendDebuggee;
           supportsDelayedStackTraceLoading;
           supportsLoadedSourcesRequest;
           supportsLogPoints;
           supportsTerminateThreadsRequest;
           supportsSetExpression;
           supportsTerminateRequest;
           supportsDataBreakpoints;
           supportsReadMemoryRequest;
         }
      )
      (obj10
         (opt "supportTerminateDebuggee" bool)
         (opt "supportSuspendDebuggee" bool)
         (opt "supportsDelayedStackTraceLoading" bool)
         (opt "supportsLoadedSourcesRequest" bool)
         (opt "supportsLogPoints" bool)
         (opt "supportsTerminateThreadsRequest" bool)
         (opt "supportsSetExpression" bool)
         (opt "supportsTerminateRequest" bool)
         (opt "supportsDataBreakpoints" bool)
         (opt "supportsReadMemoryRequest" bool)
      )

  let enc_t3 =
    let open Data_encoding in
    conv
      (fun {
         supportsWriteMemoryRequest;
         supportsDisassembleRequest;
         supportsCancelRequest;
         supportsBreakpointLocationsRequest;
         supportsClipboardContext;
         supportsSteppingGranularity;
         supportsInstructionBreakpoints;
         supportsExceptionFilterOptions;
         supportsSingleThreadExecutionRequests;
       } -> (
           supportsWriteMemoryRequest,
           supportsDisassembleRequest,
           supportsCancelRequest,
           supportsBreakpointLocationsRequest,
           supportsClipboardContext,
           supportsSteppingGranularity,
           supportsInstructionBreakpoints,
           supportsExceptionFilterOptions,
           supportsSingleThreadExecutionRequests
         )
      )
      (fun (
         supportsWriteMemoryRequest,
         supportsDisassembleRequest,
         supportsCancelRequest,
         supportsBreakpointLocationsRequest,
         supportsClipboardContext,
         supportsSteppingGranularity,
         supportsInstructionBreakpoints,
         supportsExceptionFilterOptions,
         supportsSingleThreadExecutionRequests
       ) -> {
           supportsWriteMemoryRequest;
           supportsDisassembleRequest;
           supportsCancelRequest;
           supportsBreakpointLocationsRequest;
           supportsClipboardContext;
           supportsSteppingGranularity;
           supportsInstructionBreakpoints;
           supportsExceptionFilterOptions;
           supportsSingleThreadExecutionRequests;
         }
      )
      (obj9
         (opt "supportsWriteMemoryRequest" bool)
         (opt "supportsDisassembleRequest" bool)
         (opt "supportsCancelRequest" bool)
         (opt "supportsBreakpointLocationsRequest" bool)
         (opt "supportsClipboardContext" bool)
         (opt "supportsSteppingGranularity" bool)
         (opt "supportsInstructionBreakpoints" bool)
         (opt "supportsExceptionFilterOptions" bool)
         (opt "supportsSingleThreadExecutionRequests" bool)
      )


  type t = (t0 * (t1 * (t2 * t3)))

  let make
      ?supportsConfigurationDoneRequest
      ?supportsFunctionBreakpoints
      ?supportsConditionalBreakpoints
      ?supportsHitConditionalBreakpoints
      ?supportsEvaluateForHovers
      ?exceptionBreakpointFilters
      ?supportsStepBack
      ?supportsSetVariable
      ?supportsRestartFrame
      ?supportsGotoTargetsRequest
      ?supportsStepInTargetsRequest
      ?supportsCompletionsRequest
      ?completionTriggerCharacters
      ?supportsModulesRequest
      ?additionalModuleColumns
      ?supportedChecksumAlgorithms
      ?supportsRestartRequest
      ?supportsExceptionOptions
      ?supportsValueFormattingOptions
      ?supportsExceptionInfoRequest
      ?supportTerminateDebuggee
      ?supportSuspendDebuggee
      ?supportsDelayedStackTraceLoading
      ?supportsLoadedSourcesRequest
      ?supportsLogPoints
      ?supportsTerminateThreadsRequest
      ?supportsSetExpression
      ?supportsTerminateRequest
      ?supportsDataBreakpoints
      ?supportsReadMemoryRequest
      ?supportsWriteMemoryRequest
      ?supportsDisassembleRequest
      ?supportsCancelRequest
      ?supportsBreakpointLocationsRequest
      ?supportsClipboardContext
      ?supportsSteppingGranularity
      ?supportsInstructionBreakpoints
      ?supportsExceptionFilterOptions
      ?supportsSingleThreadExecutionRequests
      () : t = (
    {
      supportsConfigurationDoneRequest;
      supportsFunctionBreakpoints;
      supportsConditionalBreakpoints;
      supportsHitConditionalBreakpoints;
      supportsEvaluateForHovers;
      exceptionBreakpointFilters;
      supportsStepBack;
      supportsSetVariable;
      supportsRestartFrame;
      supportsGotoTargetsRequest;
    }, (
      {
        supportsStepInTargetsRequest;
        supportsCompletionsRequest;
        completionTriggerCharacters;
        supportsModulesRequest;
        additionalModuleColumns;
        supportedChecksumAlgorithms;
        supportsRestartRequest;
        supportsExceptionOptions;
        supportsValueFormattingOptions;
        supportsExceptionInfoRequest;
      }, (
        {
          supportTerminateDebuggee;
          supportSuspendDebuggee;
          supportsDelayedStackTraceLoading;
          supportsLoadedSourcesRequest;
          supportsLogPoints;
          supportsTerminateThreadsRequest;
          supportsSetExpression;
          supportsTerminateRequest;
          supportsDataBreakpoints;
          supportsReadMemoryRequest;
        }, {
          supportsWriteMemoryRequest;
          supportsDisassembleRequest;
          supportsCancelRequest;
          supportsBreakpointLocationsRequest;
          supportsClipboardContext;
          supportsSteppingGranularity;
          supportsInstructionBreakpoints;
          supportsExceptionFilterOptions;
          supportsSingleThreadExecutionRequests;
        })))

  let enc =
    let open Data_encoding in
    merge_objs enc_t0 @@ merge_objs enc_t1 @@ merge_objs enc_t2 enc_t3

end


module InvalidatedAreas = struct
  type t =
    | All
    | Stacks
    | Threads
    | Variables

  let enc =
    let open Data_encoding in
    conv
      (function
        | All -> "all"
        | Stacks -> "stacks"
        | Threads -> "threads"
        | Variables -> "variables"
      )
      (function
        | "all" -> All
        | "stacks" -> Stacks
        | "threads" -> Threads
        | "variables" -> Variables
        | _ -> failwith "Unknown invalidated areas"
      )
      string
end

module SteppingGranularity = struct

  type t =
    | Statement
    | Line
    | Instruction

  let enc =
    let open Data_encoding in
    conv
      (function | Statement -> "statement" | Line -> "line" | Instruction -> "instruction")
      (function | "statement" -> Statement | "line" -> Line | "instruction" -> Instruction | _ -> failwith "Unknown stepping granularity")
      string

end


module StackFrame = struct
(* TODO rest of optional data *)
  type t = {
    id: int;
    name: string;
    line: int;
    column: int;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {id; name; line; column} -> (id, name, line, column))
      (fun (id, name, line, column) -> {id; name; line; column})
      (obj4
         (req "id" int31)
         (req "name" string)
         (req "line" int31)
         (req "column" int31))

end


module Scope = struct
(* TODO rest of optional data *)
  type t = {
    name: string;
    variablesReference: int;
    expensive: bool;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {name; variablesReference; expensive} -> (name, variablesReference, expensive))
      (fun (name, variablesReference, expensive) -> {name; variablesReference; expensive})
      (obj3
         (req "name" string)
         (req "variablesReference" int31)
         (req "expensive" bool))

end

(* TODO Variable name seems to clash with something *)
module Variable_ = struct
(* TODO rest of optional data *)
  type t = {
    name: string;
    value: string;
    variablesReference: int;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {name; value; variablesReference} -> (name, value, variablesReference))
      (fun (name, value, variablesReference) -> {name; value; variablesReference})
      (obj3
         (req "name" string)
         (req "value" string)
         (req "variablesReference" int31)
      )

end
