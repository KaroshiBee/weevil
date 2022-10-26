
type ('ic, 'oc) t = {
  backend_ic: 'ic;
  backend_oc: 'oc;
}

let from_channels backend_ic backend_oc =
  {backend_ic; backend_oc}
