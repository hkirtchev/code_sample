# code_sample

1. Semantic background

In the Ada programming language model, prior to executing the main program, all units which comprise the application must be loaded,
and any initialization code they contain executed. This action is referred to as **elaboration**.

The elaboration of a unit may contain simple initialization code, such as:
   ```
   package Simple_Code is
      Obj : constant Integer := 123;
   end Simple_Code;
   ```
Things get a bit more interesting when the initialization code contains activations of tasks, calls to subprograms, and instantiations
of generics.
   ```
   package Server is
      function Identity (Val : Integer) return Integer;  --  subprogram
      
      generic
         type Argument_Type is range <>;
      function Min                                       --  generic
        (Left  : Argument_Type;
         Right : Argument_Type) return Argument_Type;
      
      task type Printer is                               --  task
         entry Start;
      end Printer;
   end Server;
   
   package body Server is
      function Identity (Val : Integer) return Integer is
      begin
         return Val;
      end Identity;

      function Min
        (Left  : Argument_Type;
         Right : Argument_Type) return Argument_Type
      is
      begin
         return Argument_Type'Min (Left, Right);
      end Min;
      
      task body Printer is
      begin
         accept Start;
         Put_Line ("Hello from Printer");
      end Printer;
   end Server;
   
   with Server; use Server;
   package Client is
      One : constant Integer := Identity (1);            --  call of subprogram
      
      function Integer_Min is new Min (Integer);         --  instantiation of generic
      
      Pnt : Printer;                                     --  activation of task
   end Client;
   ```
In the example above, the elaboration of `Server` has no effect because it does not contain any elaboration code. The elaboration of
`Client` however calls function `Identity`, instantiates generic `Min`, creates and activates a task of type `Printer`. The Ada language
semantics mandate that the bodies of `Identity`, `Min`, and `Printer`, must be elaborated prior to their respective uses.

2. Project objectives

The aim of the project is to determine the proper elaboration order of units, given explicit dependencies between units (`with Server;`
in the example above), and implicit dependencies stemming from the activation, instantiation, or invocation of constructs (the body of
`Identity` must be elaborated prior to the call to `Identity`).

A poor elaboration order for the example above is

   1. specification of Client
   2. specification of Server
   3. body of Client

When the specification of `Server` is elaborated, `Identity` is invoked, however the body of `Identity` is not yet elaborated, leading
to a potentially malformed value of `One`.

A correct elaboration order for the example above is

   1. specification of Client
   2. body of Client
   3. specification of Server

Determining the proper elaboration order becomes quite complicated when there are thousands of units involved, each with its own set of
explicit and implicit dependencies.

3. Approach

The GNAT Pro compiler utilizes a model which analyzes one unit at a time. The compiler does not have the ASTs of all units within the
compilation, and as a result cannot perform any complex traversals of the activation/call/instantiation graph (referred to **invocation
graph**) in order to extract all implicit dependencies between units.

To circumvent this limitation, the compiler captures pieces of the invocation graph, and store them in ALI files (Ada Link Information
files). These files are then handed off to the binder, which can now reconstruct the full invocation graph to aid in determining the
elaboration order.

4. Compiler side

The capture of invocation graph pieces is done by Sem_Elab (SEMantics of ELABoration), in particular routine `Record_Invocation_Graph`.
Sem_Elab is also in charge of various other elaboration-related activities such as installation of runtime checks to catch attempts of
invoking a construct whose body has not been elaborated, emission of error diagnostics, verification of SPARK 2014 semantics, etc.

5. Binder side

The GNAT Pro binder's task (among many) is to determine the proper elaboration order for all units in the application. Once the order is
found, it is encoded in a driver which elaborates the units and executes the main program.

Bindo (BINDer Order) is in charge of discovering the proper elaboration order, or diagnosing an unsatisfiable set of dependencies. Bindo
achieves this by constructing three graphs - unit graph (referred to **library graph***), invocation graph (mentioned before), and a
strongly-connected component graph (referred to **SCC graph**).

The library graph is constructed directly from the explicit dependencies (the `with`s) in the units. Some of these explicit dependencies
can be further altered by specifying `pragma Elaborate` or `pragma Elaborate_All` which enfore additional programmer-driven constraints
on the elaboration order.

The invocation graph is constructed from the pieces encoded by Sem_Elab in ALI files. The invocation graph is the explored starting from
elaboration code, in an attempt to determine whether the code would "jump" from one unit to another. If such a jump occurs, then a new
dependency is introduced on the unit which is the target of the jump.

The SCC graph is constructed from the augmented library graph using Tarjan's SCC algorithm.

Once all three graphs are available, a modified Topological Sort is used to find the elaboration order by exploring the library graph
while using the SCC graph as an extra guide.
