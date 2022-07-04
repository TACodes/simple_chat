-record(user, {id :: backend_db:id(),
               name :: backend_db:name(),
               password :: backend_db:password(),
               messages=[] :: backend_db:messages()}). %% message with timestamp