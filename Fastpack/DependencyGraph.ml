module StringSet = Set.Make(String)
module M = Map.Make(String)

type t = {
  get_modules : unit -> string list;
  lookup_module : string -> Module.t option;
  lookup_dependencies : Module.t -> (Dependency.t * Module.t option) list;
  add_module : Module.t -> unit;
  add_dependency : Module.t -> Dependency.t * Module.t option -> unit;
  sort : Module.t -> Module.t list;
}

exception Cycle of string list

let empty () =
  let modules = ref M.empty in
  let dependencies = ref M.empty in

  let get_modules () =
    !modules |> M.bindings |> List.map fst
  in

  let lookup_module filename =
    M.get filename !modules
  in

  let lookup_dependencies (m : Module.t) =
    match M.get m.filename !dependencies with
    | None -> []
    | Some deps -> deps
  in

  let add_module (m : Module.t) =
    modules := M.add m.filename m !modules
  in

  let add_dependency (m : Module.t) (dep : (Dependency.t * Module.t option)) =
    dependencies :=
      M.update
        m.filename
        (function | None -> Some [dep] | Some deps -> Some (dep::deps))
        !dependencies
  in

  let sort entry =
    let sorted = ref [] in
    let seen_globally = ref (StringSet.empty) in
    let add_module m =
      sorted := m :: !sorted;
      seen_globally := StringSet.add m.Module.filename !seen_globally
    in
    let check_module m =
      StringSet.mem m.Module.filename !seen_globally
    in
    let rec sort seen m =
      match List.mem m.Module.filename seen with
      | true ->
        let prev_m =
          match lookup_module (List.hd seen) with
          | Some prev_m -> prev_m
          | None -> Error.ie "DependencyGraph.sort - imporssible state"
        in
        if m.Module.es_module && prev_m.Module.es_module
        then ()
        else
          let filenames = m.Module.filename :: seen in
          raise (Cycle filenames)
      | false ->
        match check_module m with
        | true -> ()
        | false ->
          let sort' = sort (m.Module.filename :: seen) in
          let () =
            List.iter
              sort'
              (List.filter_map (fun (_, m) -> m) (lookup_dependencies m))
          in
            add_module m;
    in
    begin
      sort [] entry;
      List.rev !sorted
    end
  in
  {
    get_modules;
    lookup_module;
    lookup_dependencies;
    add_module;
    add_dependency;
    sort;
  }
