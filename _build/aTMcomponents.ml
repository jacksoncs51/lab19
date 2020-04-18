
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; mutable balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)
let database = ref [] ;;
let initialize (acclst : account_spec list) =
    database := acclst
;;

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let acquire_id : id =
  print_string "What's your ID number?";
  let id = read_int () in
  id
;;

   (* acquire_amount () -- Requests from the ATM customer and returns an
      amount by prompting for an amount and reading an int from stdin. *)
let acquire_amount : int =
  print_string "Amount?";
  let amount = read_int () in
  amount
;;

   (* acquire_act () -- Requests from the user and returns an action to
      be performed, as a value of type action *)
let acquire_act : action =
  print_string "What do you want to do? +, -, B, Next, Finished";
  let act = read_line () in
    match act with
    | "+" -> Deposit acquire_amount
    | "-" -> Withdraw acquire_amount
    | "B" -> Balance
    | "Next" -> Next
    | "Finished" -> Finished
    | _ -> raise (Failure "Select a correct action")
;;

   (*....................................................................
     Querying and updating the account database

     These functions all raise Not_found if there is no account with the
     given id.
    *)

   (* get_balance id -- Returns the balance for the customer account with
      the given id. *)
let get_balance (id : id) : int =
  (List.find (fun acct -> acct.id = id) !database).balance
;;

   (* get_name id -- Returns the name associated with the customer
      account with the given id. *)
let get_name (id : id) : string =
  (List.find (fun acct -> acct.id = id) !database).name
  ;;

   (* update_balance id amount -- Modifies the balance of the customer
      account with the given id,setting it to the given amount. *)
let update_balance (id : id) (amount : int) =
  (List.find (fun acct -> acct.id = id) !database).balance <- amount
  ;;

   (*....................................................................
     Presenting information and cash to the customer
    *)

   (* present_message message -- Presents to the customer (on stdout) the
      given message followed by a newline. *)
let present_message (str: string): unit = 
  print_string str;
  print_newline();;

   (* deliver_cash amount -- Dispenses the given amount of cash to the
      customer (really just prints to stdout a message to that
      effect). *)
let deliver_cash (amt: int) : unit =
  print_string ("Cash dispensed: " ^ string_of_int amt);
  print_newline();;
