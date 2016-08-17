

deftype name() :: type_expr .


app1
defnodule test
record name
	name = ""  :: string
	age :: integer()
end
end

baron^app1-test-name.beam


module:records() -> [atom()]
module:record(size, atom() | tagtuple()) -> error() | pos_integer()
module:record(fields, atom() | tagtuple()) -> error() | [atom()]
module:record_field(types, atom() | [atom()], atom() | tagtuple()) -> error() | typeid() | [typeid()]
module:record_field(offset, atom() | [atom()], atom() | tagtuple()) -> error() | natural() | [natural()]





record$set(#record{} = X, field, value) -> X#record{ field = value }


like recless

-record r_address{ city :: string() }.
-record r_owner{ address :: r_address{} }.
-record r_project{ owner :: #r_owner{} }.

Project :: r_project{}


City = ((Project#r_project.owner)#r_owner.address)#r_address.city,
NewProject =
       Project#r_project{owner =
        (Project#r_project.person)#r_owner{address =
          ((Project#r_project.person)#r_owner.address)#r_address{city =
            'Boston'}}}.
            

City = Project.owner.address.city,
NewProject = Project.owner.address.city = 'Boston'.

NewProject = Project.owner.address#{city = "Boston"}
NewProject = Project.owner#{address = r_address()}
NewProject = Project.owner#{address = #r_address{city = "Boston"}}


record$set(Project, owner,
    record$set(Project.owner, address,
        record$set(Project.owner.address, city, "Boston") ) )




