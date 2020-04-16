(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
   val acquire_id : unit -> id ;;

   (* acquire_amount () -- Requests from the ATM customer and returns an
      amount by prompting for an amount and reading an int from stdin. *)
   val acquire_amount : unit -> int ;;
   
   (* acquire_act () -- Requests from the user and returns an action to
      be performed, as a value of type action *)
   val acquire_act : unit -> action ;;
   
   (*....................................................................
     Querying and updating the account database
   
     These functions all raise Not_found if there is no account with the
     given id. 
    *)
     
   (* get_balance id -- Returns the balance for the customer account with
      the given id. *)
   val get_balance : id -> int ;;
   
   (* get_name id -- Returns the name associated with the customer
      account with the given id. *)
   val get_name : id -> string ;;
   
   (* update_balance id amount -- Modifies the balance of the customer
      account with the given id,setting it to the given amount. *)
   val update_balance : id -> int -> unit ;;
   
   (*....................................................................
     Presenting information and cash to the customer
    *)
     
   (* present_message message -- Presents to the customer (on stdout) the
      given message followed by a newline. *)
   val present_message : string -> unit ;;
   
   (* deliver_cash amount -- Dispenses the given amount of cash to the
      customer (really just prints to stdout a message to that
      effect). *)
   val deliver_cash : int -> unit ;;
   