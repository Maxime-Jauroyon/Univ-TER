%{
open Atypes
%}

%token<int> ID
%token<string> UNIT
%token<string> VALUE
%token PONCTUATOR_ARROW
%token PONCTUATOR_DASH
%token PONCTUATOR_SEPARATOR
%token PONCTUATOR_COMMA
%token PONCTUATOR_COLON
%token PONCTUATOR_SEMICOLON
%token PONCTUATOR_ADD
%token EOF

%start<Atypes.automata> automata

%%

automata:
  | l=non_nullable_reaction_list EOF { Automata(l) }
  | EOF { failwith "unexpected empty list of reactions" }

non_nullable_reaction_list:
  | r=reaction { NonNullableReac(r) }
  | r=reaction l=non_nullable_reaction_list { NonNullableReacList(r,l) }

reaction:
  | i=ID PONCTUATOR_COLON l1=non_nullable_mol_list PONCTUATOR_ARROW l2=non_nullable_mol_list PONCTUATOR_SEPARATOR c=non_nullable_conc_list PONCTUATOR_DASH v=VALUE PONCTUATOR_SEMICOLON { Atypes.reac_table_length := !Atypes.reac_table_length + 1; Reaction(i,l1,l2,c,v) }
  | i1=ID PONCTUATOR_COLON i2=ID PONCTUATOR_SEPARATOR v=VALUE u=UNIT PONCTUATOR_SEMICOLON { Atypes.inhib_table_length := !Atypes.inhib_table_length + 1; Inhibitor(i1,i2,v,u) }

non_nullable_mol_list:
  | i=ID { NonNullableMol(i) }
  | i=ID PONCTUATOR_ADD l=non_nullable_mol_list { NonNullableMolList(i,l) }

non_nullable_conc_list:
  | v=VALUE u=UNIT { NonNullableConc(v,u) }
  | v=VALUE u=UNIT PONCTUATOR_COMMA l=non_nullable_conc_list { NonNullableConcList(v,u,l) }