type t = [
  | `Launch
  | `Attach
  | `AttachForSuspendedLaunch
] [@@deriving show]
