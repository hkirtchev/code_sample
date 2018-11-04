------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D O                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2018, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Binderr; use Binderr;
with Butil;   use Butil;
with Debug;   use Debug;
with Output;  use Output;
with Table;
with Types;   use Types;

with GNAT;                 use GNAT;
with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;
with GNAT.Lists;           use GNAT.Lists;
with GNAT.Sets;            use GNAT.Sets;

package body Bindo is

   ---------------------------------
   -- Elaboration order mechanism --
   ---------------------------------

   --  Kirtchev ??? fill this section out

   -----------------
   -- Terminology --
   -----------------

   --  Kirtchev ??? fill this section out

   --------------
   -- Switches --
   --------------

   --  -d_A  Output ALI invocation tables
   --
   --        GNATbind outputs the contents of ALI table Invocation_Constructs
   --        and Invocation_Edges in textual format to standard output.
   --
   --  -d_I  Output invocation graph
   --
   --        GNATbind outputs the invocation graph in text format to standard
   --        output.
   --
   --  -d_L  Output library graph
   --
   --        GNATbind outputs the library graph in textual format to standard
   --        output.
   --
   --  -d_N  New bindo order
   --
   --        GNATbind utilizes the new bindo elaboration order
   --
   --  -d_O  Output elaboration order
   --
   --        GNATbind outputs the elaboration order in text format to standard
   --        output.
   --
   --  -d_S  Output SCC graph
   --
   --        GNATbind outputs the SCC graph in text format to standard output
   --
   --  -d_T  Output elaboration order trace information
   --
   --        GNATbind outputs trace information on elaboration order activities
   --        to standard output.
   --
   --  -d_V  Validate bindo graphs and order
   --
   --        GNATbind validates the invocation graph, library graph, SCC graph
   --        and elaboration order by detecting inconsistencies and producing
   --        error reports.

   ----------------------------------------
   -- Debugging elaboration order issues --
   ----------------------------------------

   --  ???

   -----------
   -- Types --
   -----------

   --  The following constants are used in two-key hashing

   Half : constant := 2 ** (Bucket_Range_Type'Size / 2);
   Mask : constant := Half - 1;

   --  The following constant is used when emitting numbers aligned on a column

   Number_Alignment_Column : constant := 6;

   ---------------------------
   -- Invocation graph edge --
   ---------------------------

   --  The following type identifies an invocation graph edge

   type Invocation_Graph_Edge_Id is range 0 .. Integer'Last;

   No_Invocation_Graph_Edge    : constant Invocation_Graph_Edge_Id :=
                                   Invocation_Graph_Edge_Id'First;
   First_Invocation_Graph_Edge : constant Invocation_Graph_Edge_Id :=
                                   No_Invocation_Graph_Edge + 1;

   procedure Destroy (IGE_Id : in out Invocation_Graph_Edge_Id);
   pragma Inline (Destroy);
   --  Destroy invocation graph edge with id IGE_Id

--   function Hash
--     (IGE_Id : Invocation_Graph_Edge_Id) return Bucket_Range_Type;
--   pragma Inline (Hash);
   --  Obtain the hash value of key IGE_Id

   --  The following type represents a list of invocation graph edges

   package IGE_List is new Doubly_Linked_List
     (Element_Type    => Invocation_Graph_Edge_Id,
      "="             => "=",
      Destroy_Element => Destroy);

   ---------------------------
   -- Invocation graph node --
   ---------------------------

   --  The following type identifies an invocation graph node

   type Invocation_Graph_Node_Id is range 0 .. Integer'Last;

   No_Invocation_Graph_Node    : constant Invocation_Graph_Node_Id :=
                                   Invocation_Graph_Node_Id'First;
   First_Invocation_Graph_Node : constant Invocation_Graph_Node_Id :=
                                   No_Invocation_Graph_Node + 1;

   procedure Destroy (IGN_Id : in out Invocation_Graph_Node_Id);
   pragma Inline (Destroy);
   --  Destroy invocation graph node with id IGN_Id

   function Hash (IGN_Id : Invocation_Graph_Node_Id) return Bucket_Range_Type;
   pragma Inline (Hash);
   --  Obtain the hash value of key IGN_Id

   --  The following type represents a list of invocation graph nodes

   package IGN_List is new Doubly_Linked_List
     (Element_Type    => Invocation_Graph_Node_Id,
      "="             => "=",
      Destroy_Element => Destroy);

   --  The following type represents a set of invocation graph nodes

   package IGN_Set is new Membership_Set
     (Element_Type => Invocation_Graph_Node_Id,
      "="          => "=",
      Hash         => Hash);

   ------------------------
   -- Library graph edge --
   ------------------------

   --  The following type identifies a library graph edge

   type Library_Graph_Edge_Id is range 0 .. Integer'Last;

   No_Library_Graph_Edge    : constant Library_Graph_Edge_Id :=
                                Library_Graph_Edge_Id'First;
   First_Library_Graph_Edge : constant Library_Graph_Edge_Id :=
                                No_Library_Graph_Edge + 1;

   procedure Destroy (LGE_Id : in out Library_Graph_Edge_Id);
   pragma Inline (Destroy);
   --  Destroy library graph edge with id LGE_Id

--   function Hash (LGE_Id : Library_Graph_Edge_Id) return Bucket_Range_Type;
--   pragma Inline (Hash);
   --  Obtain the hash value of key LGE_Id

   --  The following type represents a list of library graph edges

   package LGE_List is new Doubly_Linked_List
     (Element_Type    => Library_Graph_Edge_Id,
      "="             => "=",
      Destroy_Element => Destroy);

   -------------------------
   --  Library graph node --
   -------------------------

   --  The following type identifies a library graph node

   type Library_Graph_Node_Id is range 0 .. Integer'Last;

   No_Library_Graph_Node    : constant Library_Graph_Node_Id :=
                                Library_Graph_Node_Id'First;
   First_Library_Graph_Node : constant Library_Graph_Node_Id :=
                                No_Library_Graph_Node + 1;

   procedure Destroy (LGN_Id : in out Library_Graph_Node_Id);
   pragma Inline (Destroy);
   --  Destroy library graph node with id LGN_Id

   function Hash (LGN_Id : Library_Graph_Node_Id) return Bucket_Range_Type;
   pragma Inline (Hash);
   --  Obtain the hash value of key LGN_Id

   --  The following type represents a list of library graph nodes

   package LGN_List is new Doubly_Linked_List
     (Element_Type    => Library_Graph_Node_Id,
      "="             => "=",
      Destroy_Element => Destroy);

   --  The following type represents a set of library graph nodes

   package LGN_Set is new Membership_Set
     (Element_Type => Library_Graph_Node_Id,
      "="          => "=",
      Hash         => Hash);

   --------------------
   -- SCC graph node --
   --------------------

   --  The following type identifies a SCC graph node

   type SCC_Graph_Node_Id is range 0 .. Integer'Last;

   No_SCC_Graph_Node    : constant SCC_Graph_Node_Id :=
                            SCC_Graph_Node_Id'First;
   First_SCC_Graph_Node : constant SCC_Graph_Node_Id :=
                            No_SCC_Graph_Node + 1;

   procedure Destroy (SCCGN_Id : in out SCC_Graph_Node_Id);
   pragma Inline (Destroy);
   --  Destroy SCC graph node with id SCCGN_Id

   --  The following type represents a list of SCC graph nodes

   package SCCGN_List is new Doubly_Linked_List
     (Element_Type    => SCC_Graph_Node_Id,
      "="             => "=",
      Destroy_Element => Destroy);

   -----------
   -- Units --
   -----------

   function Hash (U_Id : Unit_Id) return Bucket_Range_Type;
   --  Obtain the hash value of key U_Id

   --------------
   -- Services --
   --------------

   package ALI_Writer is

      ---------
      -- API --
      ---------

      procedure Write_ALI_Tables;
      pragma Inline (Write_ALI_Tables);
      --  ???

   end ALI_Writer;
   use ALI_Writer;

   package Elaboration_Order is

      ---------
      -- API --
      ---------

      procedure Elaborate_Units (Order : out Unit_Id_Table);
      --  ???

   end Elaboration_Order;
   use Elaboration_Order;

   package Elaboration_Order_Validator is

      ---------
      -- API --
      ---------

      Invalid_Elaboration_Order : exception;
      --  Exception raised when the elaboration order contains invalid data

      procedure Validate_Elaboration_Order (Order : Unit_Id_Table);
      pragma Inline (Validate_Elaboration_Order);
      --  ???

   end Elaboration_Order_Validator;
   use Elaboration_Order_Validator;

   package Elaboration_Order_Writer is

      ---------
      -- API --
      ---------

      procedure Write_Elaboration_Order (Order : Unit_Id_Table);
      pragma Inline (Write_Elaboration_Order);
      --  ???

   end Elaboration_Order_Writer;
   use Elaboration_Order_Writer;

   package Invocation_Graph is

      ---------
      -- API --
      ---------

      procedure Build_Invocation_Graph;
      --  Create the invocation graph

      function Invocation_Graph_Edge_Count
        (Kind : Invocation_Kind) return Natural;
      pragma Inline (Invocation_Graph_Edge_Count);
      --  ???

      function Iterate_Elaboration_Roots return IGN_List.Iterator;
      pragma Inline (Iterate_Elaboration_Roots);
      --  Obtain an iterator over all elaboration roots

      function Iterate_Invocation_Graph_Edges return IGE_List.Iterator;
      pragma Inline (Iterate_Invocation_Graph_Edges);
      --  Obtain an iterator over all invocation graph edges

      function Iterate_Invocation_Graph_Nodes return IGN_List.Iterator;
      pragma Inline (Iterate_Invocation_Graph_Nodes);
      --  Obtain an iterator over all invocation graph nodes

      function Total_Elaboration_Roots return Natural;
      pragma Inline (Total_Elaboration_Roots);
      --  Obtain the total number of elaboration roots

      function Total_Invocation_Graph_Edges return Natural;
      pragma Inline (Total_Invocation_Graph_Edges);
      --  Obtain the total number of invocation graph edges

      function Total_Invocation_Graph_Nodes return Natural;
      pragma Inline (Total_Invocation_Graph_Nodes);
      --  Obtain the total number of invocation graph nodes

      --------------------------------------
      -- Invocation graph edge attributes --
      --------------------------------------

      function Present (IGE_Id : Invocation_Graph_Edge_Id) return Boolean;
      pragma Inline (Present);
      --  Determine whether invocation graph edge with id IGE_Id exists

      function Relation
        (IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Relation_Id;
      pragma Inline (Relation);
      --  Obtain attribute Relation of invocation graph edge with id IGE_Id

      function Target
        (IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Graph_Node_Id;
      pragma Inline (Target);
      --  Obtain attribute Target of invocation graph edge with id IGE_Id

      --------------------------------------
      -- Invocation graph node attributes --
      --------------------------------------

      function Construct
        (IGN_Id : Invocation_Graph_Node_Id) return Invocation_Construct_Id;
      pragma Inline (Construct);
      --  Obtain attribute Construct of invocation graph node with id IGN_Id

      function Edges_To_Targets
        (IGN_Id : Invocation_Graph_Node_Id) return IGE_List.Instance;
      pragma Inline (Edges_To_Targets);
      --  Obtain attribute Edges_To_Targets of invocation graph node with id
      --  IGN_Id.

      function Lib_Node
        (IGN_Id : Invocation_Graph_Node_Id) return Library_Graph_Node_Id;
      pragma Inline (Lib_Node);
      --  Obtain attribute Lib_Node of invocation graph node with id IGN_Id

      function Name (IGN_Id : Invocation_Graph_Node_Id) return Name_Id;
      pragma Inline (Name);
      --  Obtain the name of the construct invocation graph node with id IGN_Id
      --  represents.

      function Present (IGN_Id : Invocation_Graph_Node_Id) return Boolean;
      pragma Inline (Present);
      --  Determine whether invocation graph node with id IGN_Id exists

      -----------------
      -- Maintenance --
      -----------------

      procedure Finalize_Invocation_Graph;
      pragma Inline (Finalize_Invocation_Graph);
      --  Finalize all internal data structures

      procedure Initialize_Invocation_Graph;
      pragma Inline (Initialize_Invocation_Graph);
      --  Initialize all internal data structures

   end Invocation_Graph;
   use Invocation_Graph;

   package Invocation_Graph_Validator is

      ---------
      -- API --
      ---------

      Invalid_Invocation_Graph : exception;
      --  Exception raised when the invocation graph contains invalid data

      procedure Validate_Invocation_Graph;
      pragma Inline (Validate_Invocation_Graph);
      --  ???

   end Invocation_Graph_Validator;
   use Invocation_Graph_Validator;

   package Invocation_Graph_Writer is

      ---------
      -- API --
      ---------

      procedure Write_Invocation_Graph;
      pragma Inline (Write_Invocation_Graph);
      --  ???

   end Invocation_Graph_Writer;
   use Invocation_Graph_Writer;

   package Library_Graph is

      -----------
      -- Types --
      -----------

      --  The following type represents the various kinds of library edges

      type Library_Graph_Edge_Kind is
        (Body_Before_Spec_Edge,
         --  Successor denotes spec, Predecessor denotes a body. This is a
         --  special kind of edge used during the generation of the SCC graph.
         --  Note that a body can never be elaborated before its spec.

         Elaborate_Edge,
         --  Successor withs Predecessor, and has pragma Elaborate for it

         Elaborate_All_Edge,
         --  Successor withs Predecessor, and has pragma Elaborate_All for it

--       Forced_Edge,
         --  Successor is forced to with Predecessor by virtue of an existing
         --  elaboration order provided in a file.

         Invocation_Edge,
         --  An invocation construct in unit Successor invokes a target in unit
         --  Predecessor.

         Spec_Before_Body_Edge,
         --  Successor denotes a body, Predecessor denotes a spec

         With_Edge,
         --  Successor withs Predecessor

         No_Edge);

      ---------
      -- API --
      ---------

      procedure Build_Library_Graph_Edge
        (Pred : Library_Graph_Node_Id;
         Succ : Library_Graph_Node_Id;
         Kind : Library_Graph_Edge_Kind);
      pragma Inline (Build_Library_Graph_Edge);
      --  Create a library graph edge of kind Kind which links predecessor Pred
      --  with successor Succ. No edge is created if Pred and Succ have already
      --  been linked.

      procedure Build_Library_Graph;
      --  Create the library graph

      function Has_Elaborate_Body
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Has_Elaborate_Body);
      --  ???

      function Is_Body (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Body);
      --  Determine whether a library graph node with id LGN_Id is a body

      function Is_Body_With_Spec
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Body_With_Spec);
      --  Determine whether a library graph node with id LGN_Id is a body with
      --  a corresponding spec.

      function Is_Internal_Unit
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Internal_Unit);
      --  ???

      function Is_Predefined_Unit
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Predefined_Unit);
      --  ???

      function Is_Preelaborated_Unit
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Preelaborated_Unit);
      --  ???

      function Is_Spec (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Spec);
      --  Determine whether a library graph node with id LGN_Id is a spec

      function Is_Spec_With_Body
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Spec_With_Body);
      --  Determine whether a library graph node with id LGN_Id is a spec with
      --  a corresponding body.

      function Iterate_Library_Graph_Edges return LGE_List.Iterator;
      pragma Inline (Iterate_Library_Graph_Edges);
      --  Obtain an iterator over all library graph edges

      function Iterate_Library_Graph_Nodes return LGN_List.Iterator;
      pragma Inline (Iterate_Library_Graph_Nodes);
      --  Obtain an iterator over all library graph nodes

      function Library_Graph_Edge_Count
        (Kind : Library_Graph_Edge_Kind) return Natural;
      pragma Inline (Library_Graph_Edge_Count);
      --  ???

      function Library_Graph_Node_Of
        (U_Id : Unit_Id) return Library_Graph_Node_Id;
      pragma Inline (Library_Graph_Node_Of);
      --  Obtain the library graph node of a unit with id U_Id

      function Proper_Body
        (LGN_Id : Library_Graph_Node_Id) return Library_Graph_Node_Id;
      pragma Inline (Proper_Body);
      --  ???

      function Proper_Spec
        (LGN_Id : Library_Graph_Node_Id) return Library_Graph_Node_Id;
      pragma Inline (Proper_Spec);
      --  ???

      procedure Remove_Body_Before_Spec_Edge
        (LGE_Id : Library_Graph_Edge_Id;
         Pred   : Library_Graph_Node_Id;
         Succ   : Library_Graph_Node_Id);
      pragma Inline (Remove_Body_Before_Spec_Edge);
      --  Delete a special Body_Before_Spec library graph edge with id LGE_Id
      --  which links predecessor Pred with successor Succ.

      procedure Remove_Body_Before_Spec_Edges;
      pragma Inline (Remove_Body_Before_Spec_Edges);
      --  Delete all special Body_Before_Spec library graph edges from internal
      --  data structures.

      function Total_Library_Graph_Edges return Natural;
      pragma Inline (Total_Library_Graph_Edges);
      --  Obtain the total number of library graph edges

      function Total_Library_Graph_Nodes return Natural;
      pragma Inline (Total_Library_Graph_Nodes);
      --  Obtain the total number of library graph nodes

      -----------------------------------
      -- Library graph edge attributes --
      -----------------------------------

      function Kind
        (LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Edge_Kind;
      pragma Inline (Kind);
      --  Obtain attribute Kind of library graph edge with id LGE_Id

      function Predecessor
        (LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Node_Id;
      pragma Inline (Predecessor);
      --  Obtain attribute Predecessor of a library graph edge with id LGE_Id

      function Present (LGE_Id : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Present);
      --  Determine whether a library graph edge with id LGE_Id exists

      function Successor
        (LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Node_Id;
      pragma Inline (Successor);
      --  Obtain attribute Successor of a library graph edge with id LGE_Id

      -----------------------------------
      -- Library graph node attributes --
      -----------------------------------

      function Corresponding_Item
        (LGN_Id : Library_Graph_Node_Id) return Library_Graph_Node_Id;
      pragma Inline (Corresponding_Item);
      --  Obtain attribute Corresponding_Item of a library graph node with id
      --  LGN_Id.

      function Edges_To_Successors
        (LGN_Id : Library_Graph_Node_Id) return LGE_List.Instance;
      pragma Inline (Edges_To_Successors);
      --  Obtain attribute Edges_To_Successors of library graph node with id
      --  LGN_Id.

      function In_Elaboration_Order
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (In_Elaboration_Order);
      --  Obtain attribute In_Elaboration_Order of a library graph node with id
      --  LGN_Id.

      function Name (LGN_Id : Library_Graph_Node_Id) return Unit_Name_Type;
      pragma Inline (Name);
      --  Obtain the name of the unit library graph node with id LGN_Id
      --  represents.

      function Pending_Predecessors
        (LGN_Id : Library_Graph_Node_Id) return Natural;
      pragma Inline (Pending_Predecessors);
      --  Obtain attribute Pending_Predecessors of a library graph node with id
      --  LGN_Id.

      function Present (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Present);
      --  Determine whether a library graph node with id LGN_Id exists

      function SCC (LGN_Id : Library_Graph_Node_Id) return SCC_Graph_Node_Id;
      pragma Inline (SCC);
      --  Obtain attribute SCC of library graph node with id LGN_Id

      procedure Set_In_Elaboration_Order
        (LGN_Id : Library_Graph_Node_Id;
         Val    : Boolean := True);
      pragma Inline (Set_In_Elaboration_Order);
      --  Set attribute In_Elaboration_Order of a library graph node with id
      --  LGN_Id to Val.

      procedure Set_Pending_Predecessors
        (LGN_Id : Library_Graph_Node_Id;
         Val    : Natural);
      pragma Inline (Set_Pending_Predecessors);
      --  Set attribute Pending_Predecessors of a library graph node with id
      --  LGN_Id to Val.

      procedure Set_SCC
        (LGN_Id : Library_Graph_Node_Id;
         Val    : SCC_Graph_Node_Id);
      pragma Inline (Set_SCC);
      --  Set attribute SCC of a library graph node with id LGN_Id to Val

      function Unit (LGN_Id : Library_Graph_Node_Id) return Unit_Id;
      pragma Inline (Unit);
      --  Obtain attribute Unit of a library graph node with id LGN_Id

      -----------------
      -- Maintenance --
      -----------------

      procedure Finalize_Library_Graph;
      pragma Inline (Finalize_Library_Graph);
      --  Finalize all internal data structures

      procedure Initialize_Library_Graph;
      pragma Inline (Initialize_Library_Graph);
      --  Initialize all internal data structures

   end Library_Graph;
   use Library_Graph;

   package Library_Graph_Augmentor is

      ---------
      -- API --
      ---------

      procedure Augment_Library_Graph;
      --  ???

   end Library_Graph_Augmentor;
   use Library_Graph_Augmentor;

   package Library_Graph_Validator is

      ---------
      -- API --
      ---------

      Invalid_Library_Graph : exception;
      --  Exception raised when the library graph contains invalid data

      procedure Validate_Library_Graph;
      pragma Inline (Validate_Library_Graph);
      --  Verify the consistency of the library graph

   end Library_Graph_Validator;
   use Library_Graph_Validator;

   package Library_Graph_Writer is

      ---------
      -- API --
      ---------

      procedure Write_Library_Graph;
      pragma Inline (Write_Library_Graph);
      --  Output the contents of the library graph in text format to standard
      --  output.

   end Library_Graph_Writer;
   use Library_Graph_Writer;

   package SCC_Graph is

      ---------
      -- API --
      ---------

      procedure Build_SCC_Graph;
      --  Create the SCC graph

      function Iterate_SCC_Graph_Nodes return SCCGN_List.Iterator;
      pragma Inline (Iterate_SCC_Graph_Nodes);
      --  Obtain an iterator over all SCC graph nodes

      function Total_SCCs return Natural;
      pragma Inline (Total_SCCs);
      --  Obtain the number of entries in table SCCs

      -------------------------------
      -- SCC graph node attributes --
      -------------------------------

      function Lib_Nodes
        (SCCGN_Id : SCC_Graph_Node_Id) return LGN_List.Instance;
      pragma Inline (Lib_Nodes);
      --  Obtain attribute Lib_Nodes of SCC graph node with id SCCGN_Id

      function Pending_Predecessors
        (SCCGN_Id : SCC_Graph_Node_Id) return Natural;
      pragma Inline (Pending_Predecessors);
      --  Obtain attribute Pending_Predecessors of a SCC graph node with id
      --  SCCGN_Id.

      function Present (SCCGN_Id : SCC_Graph_Node_Id) return Boolean;
      pragma Inline (Present);
      --  Determine whether a SCC graph node with id SCCGN_Id exists

      procedure Set_Pending_Predecessors
        (SCCGN_Id : SCC_Graph_Node_Id;
         Val      : Natural);
      pragma Inline (Set_Pending_Predecessors);
      --  Set attribute Pending_Predecessors of a SCC graph node with id
      --  SCCGN_Id to Val.

      -----------------
      -- Maintenance --
      -----------------

      procedure Finalize_SCC_Graph;
      pragma Inline (Finalize_SCC_Graph);
      --  Finalize all internal data structures

      procedure Initialize_SCC_Graph;
      pragma Inline (Initialize_SCC_Graph);
      --  Initialize all internal data structures

   end SCC_Graph;
   use SCC_Graph;

   package SCC_Graph_Validator is

      ---------
      -- API --
      ---------

      Invalid_SCC_Graph : exception;
      --  Exception raised when the SCC graph contains invalid data

      procedure Validate_SCC_Graph;
      pragma Inline (Validate_SCC_Graph);
      --  Verify the consistency of the SCC graph

   end SCC_Graph_Validator;
   use SCC_Graph_Validator;

   package SCC_Graph_Writer is

      ---------
      -- API --
      ---------

      procedure Write_SCC_Graph;
      pragma Inline (Write_SCC_Graph);
      --  Output the contents of the SCC graph in textual format to standard
      --  output.

   end SCC_Graph_Writer;
   use SCC_Graph_Writer;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Name (U_Id : Unit_Id) return Unit_Name_Type;
   pragma Inline (Name);
   --  Obtain the name of unit with id U_Id

   function Present (Nam : Name_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether name Nam exists

   function Requires_Processing (U_Id : Unit_Id) return Boolean;
   pragma Inline (Requires_Processing);
   --  Determine whether a unit with id U_Id needs to be processed for graph
   --  creation purposes.

   function Total_Units return Natural;
   pragma Inline (Total_Units);
   --  ???

   procedure Write_Num (Val : Int);
   pragma Inline (Write_Num);
   --  ???

   ----------------
   -- ALI_Writer --
   ----------------

   package body ALI_Writer is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Invocation_Construct (IC_Id : Invocation_Construct_Id);
      pragma Inline (Write_Invocation_Construct);
      --  ???

      procedure Write_Invocation_Relation (IR_Id : Invocation_Relation_Id);
      pragma Inline (Write_Invocation_Relation);
      --  ???

      procedure Write_Invocation_Signature (IS_Id : Invocation_Signature_Id);
      pragma Inline (Write_Invocation_Signature);
      --  ???

      procedure Write_Unit (U_Id : Unit_Id);
      pragma Inline (Write_Unit);
      --  ???

      procedure Write_Units;
      pragma Inline (Write_Units);
      --  ???

      --------------------------------
      -- Write_Invocation_Construct --
      --------------------------------

      procedure Write_Invocation_Construct (IC_Id : Invocation_Construct_Id) is
         pragma Assert (Present (IC_Id));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (IC_Id);

      begin
         Write_Str ("  invocation construct (IC_Id_");
         Write_Int (Int (IC_Id));
         Write_Str (")");
         Write_Eol;

         Write_Str ("    Kind = ");
         Write_Str (IC_Rec.Kind'Img);
         Write_Eol;

         Write_Str ("    Placement = ");
         Write_Str (IC_Rec.Placement'Img);
         Write_Eol;

         Write_Str ("    Signature");
         Write_Eol;

         Write_Invocation_Signature (IC_Rec.Signature);
         Write_Eol;
      end Write_Invocation_Construct;

      -------------------------------
      -- Write_Invocation_Relation --
      -------------------------------

      procedure Write_Invocation_Relation (IR_Id : Invocation_Relation_Id) is
         pragma Assert (Present (IR_Id));

         IR_Rec : Invocation_Relation_Record renames
                    Invocation_Relations.Table (IR_Id);

      begin
         Write_Str ("  invocation relation (IR_Id_");
         Write_Int (Int (IR_Id));
         Write_Str (")");
         Write_Eol;

         if Present (IR_Rec.Extra) then
            Write_Str  ("    Extra = ");
            Write_Name (IR_Rec.Extra);
         else
            Write_Str ("    Extra = none");
         end if;

         Write_Eol;
         Write_Str ("    Invoker");
         Write_Eol;

         Write_Invocation_Signature (IR_Rec.Invoker);

         Write_Str ("    Kind = ");
         Write_Str (IR_Rec.Kind'Img);
         Write_Eol;

         Write_Str ("    Target");
         Write_Eol;

         Write_Invocation_Signature (IR_Rec.Target);
         Write_Eol;
      end Write_Invocation_Relation;

      --------------------------------
      -- Write_Invocation_Signature --
      --------------------------------

      procedure Write_Invocation_Signature (IS_Id : Invocation_Signature_Id) is
         pragma Assert (Present (IS_Id));

         IS_Rec : Invocation_Signature_Record renames
                    Invocation_Signatures.Table (IS_Id);

      begin
         Write_Str ("      Column = ");
         Write_Int (Int (IS_Rec.Column));
         Write_Eol;

         Write_Str ("      Line = ");
         Write_Int (Int (IS_Rec.Line));
         Write_Eol;

         if Present (IS_Rec.Locations) then
            Write_Str  ("      Locations = ");
            Write_Name (IS_Rec.Locations);
         else
            Write_Str ("      Locations = none");
         end if;

         Write_Eol;
         Write_Str  ("      Name = ");
         Write_Name (IS_Rec.Name);
         Write_Eol;

         Write_Str  ("      Scope = ");
         Write_Name (IS_Rec.Scope);
         Write_Eol;
      end Write_Invocation_Signature;

      ----------------------
      -- Write_ALI_Tables --
      ----------------------

      procedure Write_ALI_Tables is
      begin
         --  Nothing to do when switch -d_A (output invocation tables) is not
         --  in effect.

         if not Debug_Flag_Underscore_AA then
            return;
         end if;

         Write_Str ("ALI Tables");
         Write_Eol;
         Write_Eol;

         Write_Str ("Units: ");
         Write_Num (Int (Total_Units));
         Write_Eol;
         Write_Eol;

         Write_Units;

         Write_Str ("ALI Tables end");
         Write_Eol;
         Write_Eol;
      end Write_ALI_Tables;

      ----------------
      -- Write_Unit --
      ----------------

      procedure Write_Unit (U_Id : Unit_Id) is
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames Units.Table (U_Id);

      begin
         Write_Str  ("unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (U_Rec.Uname);
         Write_Eol;

         Write_Str ("  First_Invocation_Construct (IC_Id_");
         Write_Int (Int (U_Rec.First_Invocation_Construct));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Last_Invocation_Construct  (IC_Id_");
         Write_Int (Int (U_Rec.Last_Invocation_Construct));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  First_Invocation_Relation  (IR_Id_");
         Write_Int (Int (U_Rec.First_Invocation_Relation));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Last_Invocation_Relation   (IR_Id_");
         Write_Int (Int (U_Rec.Last_Invocation_Relation));
         Write_Str (")");
         Write_Eol;
         Write_Eol;

         for IC_Id in U_Rec.First_Invocation_Construct ..
                      U_Rec.Last_Invocation_Construct
         loop
            Write_Invocation_Construct (IC_Id);
         end loop;

         for IR_Id in U_Rec.First_Invocation_Relation ..
                      U_Rec.Last_Invocation_Relation
         loop
            Write_Invocation_Relation (IR_Id);
         end loop;
      end Write_Unit;

      -----------------
      -- Write_Units --
      -----------------

      procedure Write_Units is
      begin
         for U_Id in Units.First .. Units.Last loop
            Write_Unit (U_Id);
         end loop;
      end Write_Units;
   end ALI_Writer;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (IGE_Id : in out Invocation_Graph_Edge_Id) is
      pragma Unreferenced (IGE_Id);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (IGN_Id : in out Invocation_Graph_Node_Id) is
      pragma Unreferenced (IGN_Id);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (LGE_Id : in out Library_Graph_Edge_Id) is
      pragma Unreferenced (LGE_Id);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (LGN_Id : in out Library_Graph_Node_Id) is
      pragma Unreferenced (LGN_Id);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (SCCGN_Id : in out SCC_Graph_Node_Id) is
      pragma Unreferenced (SCCGN_Id);
   begin
      null;
   end Destroy;

   -----------------------
   -- Elaboration_Order --
   -----------------------

   package body Elaboration_Order is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Add_Candidate
        (LGN_Id : Library_Graph_Node_Id;
         Set    : LGN_Set.Instance;
         Step   : Natural);
      pragma Inline (Add_Candidate);
      --  ???

      function Build_Elaborable_Set
        (Iter : in out LGN_List.Iterator;
         Size : Natural;
         Step : Natural) return LGN_Set.Instance;
      pragma Inline (Build_Elaborable_Set);
      --  ???

      function Find_Best_Candidate
        (Set  : LGN_Set.Instance;
         Step : Natural) return Library_Graph_Node_Id;
      pragma Inline (Find_Best_Candidate);
      --  Find the best candidate for elaboration purposes among all library
      --  graph nodes in set Set.

      procedure Elaborate
        (LGN_Id : Library_Graph_Node_Id;
         Set    : LGN_Set.Instance;
         Nodes  : in out Natural;
         Order  : in out Unit_Id_Table;
         Step   : Natural);
      pragma Inline (Elaborate);
      --  ???

      procedure Elaborate
        (SCCGN_Id : SCC_Graph_Node_Id;
         Nodes    : in out Natural;
         Order    : in out Unit_Id_Table;
         Step     : Natural);
      pragma Inline (Elaborate);
      --  ???

      function Is_Better_Candidate
        (Best_Candid : Library_Graph_Node_Id;
         New_Candid  : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Better_Candidate);
      --  Determine whether a candidate library graph node with id New_Candid
      --  is a better choice for elaboration purposes compared to the current
      --  best node with id Best_Candid.

      function Is_Elaborable
        (LGN_Id : Library_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Elaborable);
      --  Determine whether library graph node with id LGN_Id can be elaborated

      procedure Trace_Node (LGN_Id : Library_Graph_Node_Id);
      pragma Inline (Trace_Node);
      --  ???

      procedure Trace_SCC (SCCGN_Id : SCC_Graph_Node_Id);
      pragma Inline (Trace_SCC);
      --  ???

      procedure Trace_Step (Step : Natural);
      pragma Inline (Trace_Step);
      --  ???

      procedure Update_Successors
        (Pred : Library_Graph_Node_Id;
         Set  : LGN_Set.Instance;
         Step : Natural);
      pragma Inline (Update_Successors);
      --  ???

      -------------------
      -- Add_Candidate --
      -------------------

      procedure Add_Candidate
        (LGN_Id : Library_Graph_Node_Id;
         Set    : LGN_Set.Instance;
         Step   : Natural)
      is
      begin
         pragma Assert (Present (LGN_Id));

         --  Output extra information when switch -d_T (output elaboration
         --  order trace information is in effect).

         if Debug_Flag_Underscore_TT then
            Trace_Step (Step);
            Write_Str  ("add candidate ");
            Trace_Node (LGN_Id);
         end if;

         LGN_Set.Insert (Set, LGN_Id);
      end Add_Candidate;

      --------------------------
      -- Build_Elaborable_Set --
      --------------------------

      function Build_Elaborable_Set
        (Iter : in out LGN_List.Iterator;
         Size : Natural;
         Step : Natural) return LGN_Set.Instance
      is
         Set    : constant LGN_Set.Instance := LGN_Set.Create (Size);
         LGN_Id : Library_Graph_Node_Id;

      begin
         --  Inspect all library graph nodes

         while LGN_List.Has_Next (Iter) loop
            LGN_List.Next (Iter, LGN_Id);

            --  Add the current node to the set only when it can be elaborated

            if Is_Elaborable (LGN_Id) then
               Add_Candidate (LGN_Id, Set, Step);
            end if;
         end loop;

         return Set;
      end Build_Elaborable_Set;

      -------------------------
      -- Find_Best_Candidate --
      -------------------------

      function Find_Best_Candidate
        (Set  : LGN_Set.Instance;
         Step : Natural) return Library_Graph_Node_Id
      is
         procedure Trace_Candidate (Candid : Library_Graph_Node_Id);
         pragma Inline (Trace_Candidate);
         --  ???

         ---------------------
         -- Trace_Candidate --
         ---------------------

         procedure Trace_Candidate (Candid : Library_Graph_Node_Id) is
         begin
            --  Output extra information when switch -d_T (output elaboration
            --  order trace information is in effect).

            if Debug_Flag_Underscore_TT then
               pragma Assert (Present (Candid));

               Trace_Step (Step);
               Write_Str  ("current best candidate ");
               Trace_Node (Candid);
            end if;
         end Trace_Candidate;

         --  Local variables

         Best : Library_Graph_Node_Id;
         Curr : Library_Graph_Node_Id;
         Iter : LGN_Set.Iterator;

      --  Start of processing for Find_Best_Candidate

      begin
         --  Assume that there is no candidate

         Best := No_Library_Graph_Node;

         --  Inspect all library graph nodes in the set

         Iter := LGN_Set.Iterate (Set);
         while LGN_Set.Has_Next (Iter) loop
            LGN_Set.Next (Iter, Curr);

            --  Update the best candidate when there is no such candidate

            if not Present (Best) then
               Best := Curr;
               Trace_Candidate (Best);

            --  Update the best candidate when the current node is a better
            --  choice.

            elsif Is_Better_Candidate
                    (Best_Candid => Best,
                     New_Candid  => Curr)
            then
               Best := Curr;
               Trace_Candidate (Best);
            end if;
         end loop;

         return Best;
      end Find_Best_Candidate;

      ---------------
      -- Elaborate --
      ---------------

      procedure Elaborate
        (LGN_Id : Library_Graph_Node_Id;
         Set    : LGN_Set.Instance;
         Nodes  : in out Natural;
         Order  : in out Unit_Id_Table;
         Step   : Natural)
      is
      begin
         pragma Assert (Present (LGN_Id));

         --  Output extra information when switch -d_T (output elaboration
         --  order trace information is in effect).

         if Debug_Flag_Underscore_TT then
            Trace_Step (Step);
            Write_Str  ("elaborating node ");
            Trace_Node (LGN_Id);
         end if;

         --  Remove the library graph node from the set and update the number
         --  of nodes that need to be elaborated.

         LGN_Set.Delete (Set, LGN_Id);
         Nodes := Nodes - 1;

         --  Mark the node as chosen and add it to the elaboration order

         Set_In_Elaboration_Order (LGN_Id);
         Unit_Id_Tables.Append (Order, Unit (LGN_Id));

         --  Notify all successors and SCCs (where applicable) that they have
         --  one less predecessor to wait on. This may cause some successors to
         --  be included in the set.

         Update_Successors
           (Pred => LGN_Id,
            Set  => Set,
            Step => Step);

         --  The node denotes a spec subject to pragma Elaborate_Body. Choose
         --  the completing body for the elaboration order in order to satisfy
         --  the semantics of the pragma.

         if Is_Spec_With_Body (LGN_Id)
           and then Has_Elaborate_Body (LGN_Id)
         then
            Elaborate
              (LGN_Id => Proper_Body (LGN_Id),
               Set    => Set,
               Nodes  => Nodes,
               Order  => Order,
               Step   => Step);
         end if;

         --  Output extra information when switch -d_T (output elaboration
         --  order trace information is in effect).

         if Debug_Flag_Underscore_TT then
            Trace_Step (Step);
            Write_Str  ("elaborated node ");
            Trace_Node (LGN_Id);
         end if;
      end Elaborate;

      ---------------
      -- Elaborate --
      ---------------

      procedure Elaborate
        (SCCGN_Id : SCC_Graph_Node_Id;
         Nodes    : in out Natural;
         Order    : in out Unit_Id_Table;
         Step     : Natural)
      is
         pragma Assert (Present (SCCGN_Id));

         Nodes_Of_SCC  : constant LGN_List.Instance := Lib_Nodes (SCCGN_Id);
         Candid        : Library_Graph_Node_Id;
         Candidate_Set : LGN_Set.Instance;
         Iter          : LGN_List.Iterator;

      begin
         --  Output extra information when switch -d_T (output elaboration
         --  order trace information is in effect).

         if Debug_Flag_Underscore_TT then
            Trace_Step (Step);
            Write_Str  ("elaborating SCC ");
            Trace_SCC  (SCCGN_Id);
         end if;

         Iter := LGN_List.Iterate (Nodes_Of_SCC);

         --  Gather all candidate library graph nodes of the SCC that can
         --  be elaborated in a set.

         Candidate_Set :=
           Build_Elaborable_Set
             (Iter => Iter,
              Size => LGN_List.Size (Nodes_Of_SCC),
              Step => Step);

         --  Find the best candidate within the set

         Candid :=
           Find_Best_Candidate
             (Set  => Candidate_Set,
              Step => Step);

         --  Continue to elaborate the current best candidate until either the
         --  set is exhausted, or the SCC contains a circularity that prevents
         --  all of its nodes to be elaborated. Elaborating the candidate may
         --  cause some of its successors to be included in the set.

         while Present (Candid) loop
            Elaborate
              (LGN_Id => Candid,
               Set    => Candidate_Set,
               Nodes  => Nodes,
               Order  => Order,
               Step   => Step);

            Candid :=
              Find_Best_Candidate
                (Set  => Candidate_Set,
                 Step => Step);
         end loop;

         LGN_Set.Destroy (Candidate_Set);

         --  Output extra information when switch -d_T (output elaboration
         --  order trace information is in effect).

         if Debug_Flag_Underscore_TT then
            Trace_Step (Step);
            Write_Str  ("elaborated SCC ");
            Trace_SCC  (SCCGN_Id);
         end if;
      end Elaborate;

      ---------------------
      -- Elaborate_Units --
      ---------------------

      procedure Elaborate_Units (Order : out Unit_Id_Table) is
         Candid          : Library_Graph_Node_Id;
         Candidate_Set   : LGN_Set.Instance;
         Iter            : LGN_List.Iterator;
         Remaining_Nodes : Natural;
         Step            : Natural;

      begin
         --  Kirtchev ??? DISABLE FOR NOW

         if True then
            return;
         end if;

         Iter := Iterate_Library_Graph_Nodes;
         Step := 0;

         --  Output extra information when switch -d_T (output elaboration
         --  order trace information is in effect).

         if Debug_Flag_Underscore_TT then
            Trace_Step (Step);
            Write_Str  ("elaborating units");
            Write_Eol;
         end if;

         --  Gather all candidate library graph nodes that can be elaborated in
         --  a set.

         Candidate_Set :=
           Build_Elaborable_Set
             (Iter => Iter,
              Size => Total_Library_Graph_Nodes,
              Step => Step);

         --  The remaining nodes that must be chosen for the elaboration order
         --  are all nodes.

         Remaining_Nodes := Total_Library_Graph_Nodes;

         --  Find the best candidate among within the set

         Candid :=
           Find_Best_Candidate
             (Set  => Candidate_Set,
              Step => Step);

         --  Continue to elaborate the current best candidate along with its
         --  SCC until either the set is exhausted, or the library graph has
         --  a circularity that prevents all nodes from being elaborated.

         while Present (Candid) loop
            Step := Step + 1;

            Elaborate
              (LGN_Id => Candid,
               Set    => Candidate_Set,
               Nodes  => Remaining_Nodes,
               Order  => Order,
               Step   => Step);

            Elaborate
              (SCCGN_Id => SCC (Candid),
               Nodes    => Remaining_Nodes,
               Order    => Order,
               Step     => Step);

            Candid :=
              Find_Best_Candidate
                (Set  => Candidate_Set,
                 Step => Step);
         end loop;

         LGN_Set.Destroy (Candidate_Set);

         --  Output extra information when switch -d_T (output elaboration
         --  order trace information is in effect).

         if Debug_Flag_Underscore_TT then
            Trace_Step (Step);
            Write_Str  ("elaborated units");
            Write_Eol;
         end if;

         --  The lack of remaining nodes indicates that all nodes have been
         --  elaborated.

         if Remaining_Nodes = 0 then
            Validate_Elaboration_Order (Order);
            Write_Elaboration_Order (Order);

         --  Otherwise an elaboration circularity prevents the remaining nodes
         --  from being elaborated.

         else
            Error_Msg ("elaboration circularity detected");
            raise Unrecoverable_Error;
         end if;
      end Elaborate_Units;

      -------------------------
      -- Is_Better_Candidate --
      -------------------------

      function Is_Better_Candidate
        (Best_Candid : Library_Graph_Node_Id;
         New_Candid  : Library_Graph_Node_Id) return Boolean
      is
      begin
         pragma Assert (Present (Best_Candid));
         pragma Assert (Present (New_Candid));

         --  Prefer a predefined unit over a non-predefined unit

         if Is_Predefined_Unit (Best_Candid)
           and then not Is_Predefined_Unit (New_Candid)
         then
            return False;

         elsif not Is_Predefined_Unit (Best_Candid)
           and then Is_Predefined_Unit (New_Candid)
         then
            return True;

         --  Prefer an internal unit over a non-iternal unit

         elsif Is_Internal_Unit (Best_Candid)
           and then not Is_Internal_Unit (New_Candid)
         then
            return False;

         elsif not Is_Internal_Unit (Best_Candid)
           and then Is_Internal_Unit (New_Candid)
         then
            return True;

         --  Prefer a preelaborated unit over a non-preelaborated unit

         elsif Is_Preelaborated_Unit (Best_Candid)
           and then not Is_Preelaborated_Unit (New_Candid)
         then
            return False;

         elsif not Is_Preelaborated_Unit (Best_Candid)
           and then Is_Preelaborated_Unit (New_Candid)
         then
            return True;

         --  Otherwise default to lexicographical order to ensure deterministic
         --  behavior.

         else
            return Uname_Less (Name (Best_Candid), Name (New_Candid));
         end if;
      end Is_Better_Candidate;

      -------------------
      -- Is_Elaborable --
      -------------------

      function Is_Elaborable (LGN_Id : Library_Graph_Node_Id) return Boolean is
      begin
         pragma Assert (Present (LGN_Id));

         --  A library graph node can be elaborated when
         --
         --    * The node has not been elaborated yet, and
         --
         --    * The node is no longer waiting on any of its predecessors to be
         --      elaborated, and
         --
         --    * The SCC where the node resides is no longer waiting for any of
         --      its predecessor SCCs to be elaborated.

         return
           not In_Elaboration_Order (LGN_Id)
             and then Pending_Predecessors (LGN_Id) = 0
             and then Pending_Predecessors (SCC (LGN_Id)) = 0;
      end Is_Elaborable;

      ----------------
      -- Trace_Node --
      ----------------

      procedure Trace_Node (LGN_Id : Library_Graph_Node_Id) is
      begin
         Write_Str  ("(LGN_Id_");
         Write_Int  (Int (LGN_Id));
         Write_Str  (" name = ");
         Write_Name (Name (Unit (LGN_Id)));
         Write_Eol;
      end Trace_Node;

      ---------------
      -- Trace_SCC --
      ---------------

      procedure Trace_SCC (SCCGN_Id : SCC_Graph_Node_Id) is
      begin
         Write_Str ("(SCCGN_Id_");
         Write_Int (Int (SCCGN_Id));
         Write_Eol;
      end Trace_SCC;

      ----------------
      -- Trace_Step --
      ----------------

      procedure Trace_Step (Step : Natural) is
      begin
         Write_Int (Int (Step));
         Write_Str (": ");
      end Trace_Step;

      -----------------------
      -- Update_Successors --
      -----------------------

      procedure Update_Successors
        (Pred : Library_Graph_Node_Id;
         Set  : LGN_Set.Instance;
         Step : Natural)
      is
         pragma Assert (Present (Pred));

         Pred_SCC : constant SCC_Graph_Node_Id := SCC (Pred);
         pragma Assert (Present (Pred_SCC));

         procedure Update_Successor (Succ : Library_Graph_Node_Id);
         pragma Inline (Update_Successor);
         --  Update the status of successor library graph node Succ and its SCC

         ----------------------
         -- Update_Successor --
         ----------------------

         procedure Update_Successor (Succ : Library_Graph_Node_Id) is
            pragma Assert (Present (Succ));

            Succ_SCC : constant SCC_Graph_Node_Id := SCC (Succ);
            pragma Assert (Present (Succ_SCC));

         begin
            --  Output extra information when switch -d_T (output elaboration
            --  order trace information is in effect).

            if Debug_Flag_Underscore_TT then
               Trace_Step (Step);
               Write_Str  ("updating successor ");
               Trace_Node (Succ);
            end if;

            --  The successor has one less predecessor to wait on

            Set_Pending_Predecessors (Succ, Pending_Predecessors (Succ) - 1);

            --  The predecessor and successor reside in different SCCs. The SCC
            --  of the successor has one less predecessor to wait on.

            if Succ_SCC /= Pred_SCC then
               Set_Pending_Predecessors
                 (Succ_SCC, Pending_Predecessors (Succ_SCC) - 1);
            end if;

            --  Add the successor to the set only when it can be elaborated

            if Is_Elaborable (Succ) then
               LGN_Set.Insert (Set, Succ);
            end if;

            --  Output extra information when switch -d_T (output elaboration
            --  order trace information is in effect).

            if Debug_Flag_Underscore_TT then
               Trace_Step (Step);
               Write_Str  ("updated successor ");
               Trace_Node (Succ);
            end if;
         end Update_Successor;

         --  Local variables

         Iter   : LGE_List.Iterator;
         LGE_Id : Library_Graph_Edge_Id;

      --  Start of processing for Update_Successors

      begin
         --  Inspect all successors

         Iter := LGE_List.Iterate (Edges_To_Successors (Pred));
         while LGE_List.Has_Next (Iter) loop
            LGE_List.Next (Iter, LGE_Id);
            pragma Assert (Present (LGE_Id));

            Update_Successor (Successor (LGE_Id));
         end loop;
      end Update_Successors;
   end Elaboration_Order;

   ---------------------------------
   -- Elaboration_Order_Validator --
   ---------------------------------

   package body Elaboration_Order_Validator is

      Has_Invalid_Data : Boolean := False;
      --  Flag set when the elaboration order contains invalid data

      ---------------------
      -- Data structures --
      ---------------------

      package Unit_Id_Set is new Membership_Set
        (Element_Type => Unit_Id,
         "="          => "=",
         Hash         => Hash);

      -----------------------
      -- Local subprograms --
      -----------------------

      function Build_Unit_Set return Unit_Id_Set.Instance;
      pragma Inline (Build_Unit_Set);
      --  ???

      procedure Report_Elaborated_Unit (U_Id : Unit_Id);
      pragma Inline (Report_Elaborated_Unit);
      --  ???

      procedure Report_Unelaborated_Unit (U_Id : Unit_Id);
      pragma Inline (Report_Unelaborated_Unit);
      --  ???

      procedure Report_Unelaborated_Units (Req_Set : Unit_Id_Set.Instance);
      pragma Inline (Report_Unelaborated_Units);
      --  ???

      procedure Validate_Unit (U_Id : Unit_Id; Req_Set : Unit_Id_Set.Instance);
      pragma Inline (Validate_Unit);
      --  ???

      procedure Validate_Units (Order : Unit_Id_Table);
      pragma Inline (Validate_Units);
      --  ???

      procedure Write_Error (Msg : String);
      pragma Inline (Write_Error);
      --  ???

      --------------------
      -- Build_Unit_Set --
      --------------------

      function Build_Unit_Set return Unit_Id_Set.Instance is
         Set : Unit_Id_Set.Instance;

      begin
         Set := Unit_Id_Set.Create (Total_Units);

         --  Gather all units that require elaboration in a set

         for U_Id in Units.First .. Units.Last loop
            if Requires_Processing (U_Id) then
               Unit_Id_Set.Insert (Set, U_Id);
            end if;
         end loop;

         return Set;
      end Build_Unit_Set;

      ----------------------------
      -- Report_Elaborated_Unit --
      ----------------------------

      procedure Report_Elaborated_Unit (U_Id : Unit_Id) is
         Msg : constant String := "Report_Elaborated_Unit";

      begin
         pragma Assert (Present (U_Id));

         Write_Error (Msg);

         Write_Str  ("unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (Name (U_Id));
         Write_Str  (" must not elaborated");
      end Report_Elaborated_Unit;

      ------------------------------
      -- Report_Unelaborated_Unit --
      ------------------------------

      procedure Report_Unelaborated_Unit (U_Id : Unit_Id) is
         Msg : constant String := "Report_Unelaborated_Unit";

      begin
         pragma Assert (Present (U_Id));

         Write_Error (Msg);

         Write_Str  ("unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (Name (U_Id));
         Write_Str  (" requires elaboration");
         Write_Eol;
      end Report_Unelaborated_Unit;

      -------------------------------
      -- Report_Unelaborated_Units --
      -------------------------------

      procedure Report_Unelaborated_Units (Req_Set : Unit_Id_Set.Instance) is
         Iter : Unit_Id_Set.Iterator;
         U_Id : Unit_Id;

      begin
         Iter := Unit_Id_Set.Iterate (Req_Set);
         while Unit_Id_Set.Has_Next (Iter) loop
            Unit_Id_Set.Next (Iter, U_Id);

            Report_Unelaborated_Unit (U_Id);
         end loop;
      end Report_Unelaborated_Units;

      --------------------------------
      -- Validate_Elaboration_Order --
      --------------------------------

      procedure Validate_Elaboration_Order (Order : Unit_Id_Table) is
      begin
         --  Nothing to do when switch -d_V (validate bindo graphs and order)
         --  is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Validate_Units (Order);

         if Has_Invalid_Data then
            raise Invalid_Elaboration_Order;
         end if;
      end Validate_Elaboration_Order;

      -------------------
      -- Validate_Unit --
      -------------------

      procedure Validate_Unit
        (U_Id    : Unit_Id;
         Req_Set : Unit_Id_Set.Instance)
      is
      begin
         pragma Assert (Present (U_Id));

         --  The current unit in the elaboration order appears within the set
         --  of units that require elaboration. Remove it from the set.

         if Unit_Id_Set.Contains (Req_Set, U_Id) then
            Unit_Id_Set.Delete (Req_Set, U_Id);

         --  Otherwise the current unit in the elaboration order must not be
         --  elaborated.

         else
            Report_Elaborated_Unit (U_Id);
         end if;
      end Validate_Unit;

      --------------------
      -- Validate_Units --
      --------------------

      procedure Validate_Units (Order : Unit_Id_Table) is
         Req_Set : Unit_Id_Set.Instance;

      begin
         Req_Set := Build_Unit_Set;

         for Index in Unit_Id_Tables.First ..  Unit_Id_Tables.Last (Order) loop
            Validate_Unit
              (U_Id    => Order.Table (Index),
               Req_Set => Req_Set);
         end loop;

         Report_Unelaborated_Units (Req_Set);
         Unit_Id_Set.Destroy (Req_Set);
      end Validate_Units;

      -----------------
      -- Write_Error --
      -----------------

      procedure Write_Error (Msg : String) is
      begin
         Has_Invalid_Data := True;

         Write_Str ("ERROR: ");
         Write_Str (Msg);
         Write_Eol;
      end Write_Error;
   end Elaboration_Order_Validator;

   ------------------------------
   -- Elaboration_Order_Writer --
   ------------------------------

   package body Elaboration_Order_Writer is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Unit (U_Id : Unit_Id);
      pragma Inline (Write_Unit);
      --  ???

      procedure Write_Units (Order : Unit_Id_Table);
      pragma Inline (Write_Units);
      --  ???

      -----------
      -- Debug --
      -----------

      procedure pu (U_Id : Unit_Id) renames Write_Unit;
      pragma Unreferenced (pu);

      -----------------------------
      -- Write_Elaboration_Order --
      -----------------------------

      procedure Write_Elaboration_Order (Order : Unit_Id_Table) is
      begin
         --  Nothing to do when switch -d_O (output elaboration order) is not
         --  in effect.

         if not Debug_Flag_Underscore_OO then
            return;
         end if;

         Write_Str ("Elaboration Order");
         Write_Eol;
         Write_Eol;

         Write_Units (Order);

         Write_Str ("Elaboration Order end");
         Write_Eol;

         Write_Eol;
      end Write_Elaboration_Order;

      ----------------
      -- Write_Unit --
      ----------------

      procedure Write_Unit (U_Id : Unit_Id) is
      begin
         pragma Assert (Present (U_Id));

         Write_Str  ("unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (Name (U_Id));
         Write_Eol;
      end Write_Unit;

      -----------------
      -- Write_Units --
      -----------------

      procedure Write_Units (Order : Unit_Id_Table) is
      begin
         for Index in Unit_Id_Tables.First ..  Unit_Id_Tables.Last (Order) loop
            Write_Unit (Order.Table (Index));
         end loop;
      end Write_Units;
   end Elaboration_Order_Writer;

   ----------------------------
   -- Find_Elaboration_Order --
   ----------------------------

   procedure Find_Elaboration_Order
     (Order               : out Unit_Id_Table;
      First_Main_Lib_File : File_Name_Type)
   is
      pragma Unreferenced (Order, First_Main_Lib_File);

   begin
      --  Kirtchev ??? to be removed

      if Debug_Flag_Underscore_NN then

         --  Tracing: output the ALI invocation-related tables in text format
         --  to standard output.

         Write_ALI_Tables;

         --  Create all internal data structures

         Initialize_Invocation_Graph;
         Initialize_Library_Graph;
         Initialize_SCC_Graph;

         Build_Library_Graph;
         Build_Invocation_Graph;
         Augment_Library_Graph;
         Build_SCC_Graph;
         Elaborate_Units (Order);

         --  Destroy all internal data structures

         Finalize_Invocation_Graph;
         Finalize_Library_Graph;
         Finalize_SCC_Graph;
      end if;
   end Find_Elaboration_Order;

   ----------
   -- Hash --
   ----------

   function Hash
     (IGN_Id : Invocation_Graph_Node_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (IGN_Id));
      return Bucket_Range_Type (IGN_Id);
   end Hash;

   ----------
   -- Hash --
   ----------

--   function Hash (LGE_Id : Library_Graph_Edge_Id) return Bucket_Range_Type is
--   begin
--      pragma Assert (Present (LGE_Id));
--      return Bucket_Range_Type (LGE_Id);
--   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (LGN_Id : Library_Graph_Node_Id) return Bucket_Range_Type is
   begin
      pragma Assert (Present (LGN_Id));
      return Bucket_Range_Type (LGN_Id);
   end Hash;

   ----------
   -- Hash --
   ----------

--   function Hash (SCCGN_Id : SCC_Graph_Node_Id) return Bucket_Range_Type is
--   begin
--      pragma Assert (Present (SCCGN_Id));
--      return Bucket_Range_Type (SCCGN_Id);
--   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (U_Id : Unit_Id) return Bucket_Range_Type is
   begin
      pragma Assert (Present (U_Id));
      return Bucket_Range_Type (U_Id);
   end Hash;

   ----------------------
   -- Invocation_Graph --
   ----------------------

   package body Invocation_Graph is

      ---------------------------
      -- Invocation graph edge --
      ---------------------------

      type Invocation_Graph_Edge_Record is record
         Relation : Invocation_Relation_Id := No_Invocation_Relation;
         --  Reference to the invocation relation this edge represents

         Target : Invocation_Graph_Node_Id := No_Invocation_Graph_Node;
         --  Reference to the invoked target
      end record;

      ---------------------------
      -- Invocation graph node --
      ---------------------------

      type Invocation_Graph_Node_Record is record
         Construct : Invocation_Construct_Id := No_Invocation_Construct;
         --  Reference to the invocation construct this node represents

         Edges_To_Targets : IGE_List.Instance := IGE_List.Nil;
         --  List of all invocation graph edges which lead to a target

         Lib_Node : Library_Graph_Node_Id := No_Library_Graph_Node;
         --  Reference to the library graph node where this node appears
      end record;

      ----------------
      -- Statistics --
      ----------------

      Invocation_Graph_Edge_Counts : array (Invocation_Kind) of Natural;

      ---------------------
      -- Data structures --
      ---------------------

      --  The following table stores the attributes of all invocation graph
      --  edges.

      package Edge_Attributes is new Table.Table
        (Table_Index_Type     => Invocation_Graph_Edge_Id,
         Table_Component_Type => Invocation_Graph_Edge_Record,
         Table_Low_Bound      => First_Invocation_Graph_Edge,
         Table_Initial        => 500,
         Table_Increment      => 200,
         Table_Name           => "Edge_Attributes");

      --  The following list stores all invocation graph edges in iterable
      --  form.

      Iterable_Edges : IGE_List.Instance := IGE_List.Nil;

      --  The following table stores the attributes of all invocation graph
      --  nodes.

      package Node_Attributes is new Table.Table
        (Table_Index_Type     => Invocation_Graph_Node_Id,
         Table_Component_Type => Invocation_Graph_Node_Record,
         Table_Low_Bound      => First_Invocation_Graph_Node,
         Table_Initial        => 200,
         Table_Increment      => 200,
         Table_Name           => "Node_Attributes");

      Iterable_Nodes : IGN_List.Instance := IGN_List.Nil;
      --  The iterable form of all invocation graph nodes

      Elaboration_Roots_Set : IGN_Set.Instance := IGN_Set.Nil;
      --  The set of all invocation graph nodes that represent elaboration
      --  roots.

      Iterable_Elaboration_Roots : IGN_List.Instance := IGN_List.Nil;
      --  The iterable form of all elaboration roots

      --  The following map relates invocation signatures to invocation graph
      --  nodes.

      function Hash (IS_Id : Invocation_Signature_Id) return Bucket_Range_Type;
      --  Obtain the hash value of key IS_Id

      package SN is new Dynamic_HTable
        (Key_Type              => Invocation_Signature_Id,
         Value_Type            => Invocation_Graph_Node_Id,
         No_Value              => No_Invocation_Graph_Node,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         Destroy_Value         => Destroy,
         "="                   => "=",
         Hash                  => Hash);

      Signature_To_Node_Map : SN.Instance := SN.Nil;

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Increment_Invocation_Graph_Edge_Count (Kind : Invocation_Kind);
      pragma Inline (Increment_Invocation_Graph_Edge_Count);
      --  ???

      function Invocation_Graph_Node_Of
        (IS_Id : Invocation_Signature_Id) return Invocation_Graph_Node_Id;
      pragma Inline (Invocation_Graph_Node_Of);
      --  Obtain the invocation graph node of an invocation signature with id
      --  IS_Id.

      function Is_Elaboration_Root
        (IGN_Id : Invocation_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Elaboration_Root);
      --  Determine whether an invocation graph node with id IGN_Id denotes a
      --  spec or body elaboration procedure.

      function Is_Recorded_Elaboration_Root
        (IGN_Id : Invocation_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Recorded_Elaboration_Root);
      --  Determine whether an invocation graph node with id IGN_Id is already
      --  in the set of elaboration roots.

      procedure Process_Invocation_Construct
        (IC_Id       : Invocation_Construct_Id;
         Unit_LGN_Id : Library_Graph_Node_Id);
      pragma Inline (Process_Invocation_Construct);
      --  ???

      procedure Process_Invocation_Constructs (U_Id : Unit_Id);
      pragma Inline (Process_Invocation_Constructs);
      --  ???

      procedure Process_Invocation_Relation (IR_Id : Invocation_Relation_Id);
      pragma Inline (Process_Invocation_Relation);
      --  ???

      procedure Process_Invocation_Relations (U_Id : Unit_Id);
      pragma Inline (Process_Invocation_Relations);
      --  ???

      procedure Process_Units;
      pragma Inline (Process_Units);
      --  Process all units for graph creation purposes

      procedure Save_Elaboration_Root (IGN_Id : Invocation_Graph_Node_Id);
      pragma Inline (Save_Elaboration_Root);
      --  ???

      procedure Set_Construct
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Invocation_Construct_Id);
      pragma Inline (Set_Construct);
      --  ???

      procedure Set_Is_Recorded_Elaboration_Root
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Boolean := True);
      pragma Inline (Set_Is_Recorded_Elaboration_Root);
      --  ???

      procedure Set_Lib_Node
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Library_Graph_Node_Id);
      pragma Inline (Set_Lib_Node);
      --  ???

      ----------------------------
      -- Build_Invocation_Graph --
      ----------------------------

      procedure Build_Invocation_Graph is
      begin
         Process_Units;

         Validate_Invocation_Graph;
         Write_Invocation_Graph;
      end Build_Invocation_Graph;

      ---------------
      -- Construct --
      ---------------

      function Construct
        (IGN_Id : Invocation_Graph_Node_Id) return Invocation_Construct_Id
      is
      begin
         pragma Assert (Present (IGN_Id));
         return Node_Attributes.Table (IGN_Id).Construct;
      end Construct;

      ----------------------
      -- Edges_To_Targets --
      ----------------------

      function Edges_To_Targets
        (IGN_Id : Invocation_Graph_Node_Id) return IGE_List.Instance
      is
      begin
         pragma Assert (Present (IGN_Id));
         return Node_Attributes.Table (IGN_Id).Edges_To_Targets;
      end Edges_To_Targets;

      -------------------------------
      -- Finalize_Invocation_Graph --
      -------------------------------

      procedure Finalize_Invocation_Graph is
      begin
         IGE_List.Destroy (Iterable_Edges);
         IGN_List.Destroy (Iterable_Elaboration_Roots);
         IGN_List.Destroy (Iterable_Nodes);

         IGN_Set.Destroy (Elaboration_Roots_Set);

         SN.Destroy (Signature_To_Node_Map);
      end Finalize_Invocation_Graph;

      ----------
      -- Hash --
      ----------

      function Hash
        (IS_Id : Invocation_Signature_Id) return Bucket_Range_Type
      is
      begin
         pragma Assert (Present (IS_Id));
         return Bucket_Range_Type (IS_Id);
      end Hash;

      -------------------------------------------
      -- Increment_Invocation_Graph_Edge_Count --
      -------------------------------------------

      procedure Increment_Invocation_Graph_Edge_Count
        (Kind : Invocation_Kind)
      is
         Count : Natural renames Invocation_Graph_Edge_Counts (Kind);

      begin
         Count := Count + 1;
      end Increment_Invocation_Graph_Edge_Count;

      ---------------------------------
      -- Initialize_Invocation_Graph --
      ---------------------------------

      procedure Initialize_Invocation_Graph is
      begin
         Iterable_Edges             := IGE_List.Create;
         Iterable_Elaboration_Roots := IGN_List.Create;
         Iterable_Nodes             := IGN_List.Create;

         Elaboration_Roots_Set := IGN_Set.Create (20);
         Signature_To_Node_Map := SN.Create (200);
      end Initialize_Invocation_Graph;

      ---------------------------------
      -- Invocation_Graph_Edge_Count --
      ---------------------------------

      function Invocation_Graph_Edge_Count
        (Kind : Invocation_Kind) return Natural
      is
      begin
         return Invocation_Graph_Edge_Counts (Kind);
      end Invocation_Graph_Edge_Count;

      ------------------------------
      -- Invocation_Graph_Node_Of --
      ------------------------------

      function Invocation_Graph_Node_Of
        (IS_Id : Invocation_Signature_Id) return Invocation_Graph_Node_Id
      is
         IGN_Id : Invocation_Graph_Node_Id;

      begin
         pragma Assert (Present (IS_Id));

         IGN_Id := SN.Get (Signature_To_Node_Map, IS_Id);

         --  The invocation signature lacks an invocation graph node. This
         --  indicates that the signature is enountered for the first time.

         if not Present (IGN_Id) then

            --  Create the node attributes

            Node_Attributes.Append
              ((Construct        => No_Invocation_Construct,
                Edges_To_Targets => IGE_List.Create,
                Lib_Node         => No_Library_Graph_Node));
            IGN_Id := Node_Attributes.Last;

            --  Add the node to the list of iterable nodes

            IGN_List.Append (Iterable_Nodes, IGN_Id);

            --  Associate the node with the signature

            SN.Put (Signature_To_Node_Map, IS_Id, IGN_Id);
         end if;

         return IGN_Id;
      end Invocation_Graph_Node_Of;

      -------------------------
      -- Is_Elaboration_Root --
      -------------------------

      function Is_Elaboration_Root
        (IGN_Id : Invocation_Graph_Node_Id) return Boolean
      is
         pragma Assert (Present (IGN_Id));
         pragma Assert (Present (Construct (IGN_Id)));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (Construct (IGN_Id));

      begin
         return
           IC_Rec.Kind = Elaborate_Body_Procedure
             or else
           IC_Rec.Kind = Elaborate_Spec_Procedure;
      end Is_Elaboration_Root;

      ----------------------------------
      -- Is_Recorded_Elaboration_Root --
      ----------------------------------

      function Is_Recorded_Elaboration_Root
        (IGN_Id : Invocation_Graph_Node_Id) return Boolean
      is
      begin
         pragma Assert (Present (IGN_Id));
         return IGN_Set.Contains (Elaboration_Roots_Set, IGN_Id);
      end Is_Recorded_Elaboration_Root;

      -------------------------------
      -- Iterate_Elaboration_Roots --
      -------------------------------

      function Iterate_Elaboration_Roots return IGN_List.Iterator is
      begin
         return IGN_List.Iterate (Iterable_Elaboration_Roots);
      end Iterate_Elaboration_Roots;

      ------------------------------------
      -- Iterate_Invocation_Graph_Edges --
      ------------------------------------

      function Iterate_Invocation_Graph_Edges return IGE_List.Iterator is
      begin
         return IGE_List.Iterate (Iterable_Edges);
      end Iterate_Invocation_Graph_Edges;

      ------------------------------------
      -- Iterate_Invocation_Graph_Nodes --
      ------------------------------------

      function Iterate_Invocation_Graph_Nodes return IGN_List.Iterator is
      begin
         return IGN_List.Iterate (Iterable_Nodes);
      end Iterate_Invocation_Graph_Nodes;

      --------------
      -- Lib_Node --
      --------------

      function Lib_Node
        (IGN_Id : Invocation_Graph_Node_Id) return Library_Graph_Node_Id
      is
      begin
         pragma Assert (Present (IGN_Id));
         return Node_Attributes.Table (IGN_Id).Lib_Node;
      end Lib_Node;

      ----------
      -- Name --
      ----------

      function Name (IGN_Id : Invocation_Graph_Node_Id) return Name_Id is
         pragma Assert (Present (IGN_Id));
         pragma Assert (Present (Construct (IGN_Id)));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (Construct (IGN_Id));

         pragma Assert (Present (IC_Rec.Signature));

         IS_Rec : Invocation_Signature_Record renames
                    Invocation_Signatures.Table (IC_Rec.Signature);

      begin
         return IS_Rec.Name;
      end Name;

      -------------
      -- Present --
      -------------

      function Present (IGE_Id : Invocation_Graph_Edge_Id) return Boolean is
      begin
         return IGE_Id /= No_Invocation_Graph_Edge;
      end Present;

      -------------
      -- Present --
      -------------

      function Present (IGN_Id : Invocation_Graph_Node_Id) return Boolean is
      begin
         return IGN_Id /= No_Invocation_Graph_Node;
      end Present;

      ----------------------------------
      -- Process_Invocation_Construct --
      ----------------------------------

      procedure Process_Invocation_Construct
        (IC_Id       : Invocation_Construct_Id;
         Unit_LGN_Id : Library_Graph_Node_Id)
      is
         pragma Assert (Present (IC_Id));
         pragma Assert (Present (Unit_LGN_Id));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (IC_Id);
         IGN_Id : Invocation_Graph_Node_Id;
         LGN_Id : Library_Graph_Node_Id;

      begin
         --  Determine the proper library graph node which holds the body of
         --  the invocation construct.

         if IC_Rec.Placement = In_Body then
            LGN_Id := Proper_Body (Unit_LGN_Id);
         else
            pragma Assert (IC_Rec.Placement = In_Spec);
            LGN_Id := Proper_Spec (Unit_LGN_Id);
         end if;

         --  Create an node which represents in invocation construct

         IGN_Id := Invocation_Graph_Node_Of (IC_Rec.Signature);

         Set_Construct (IGN_Id, IC_Id);
         Set_Lib_Node  (IGN_Id, LGN_Id);

         --  Save the node for later processing when it denotes a spec or body
         --  elaboration procedure.

         Save_Elaboration_Root (IGN_Id);
      end Process_Invocation_Construct;

      -----------------------------------
      -- Process_Invocation_Constructs --
      -----------------------------------

      procedure Process_Invocation_Constructs (U_Id : Unit_Id) is
         pragma Assert (Present (U_Id));

         U_Rec       : Unit_Record renames Units.Table (U_Id);
         Unit_LGN_Id : constant Library_Graph_Node_Id :=
                         Library_Graph_Node_Of (U_Id);

      begin
         --  Process each invocation construct of the unit

         for IC_Id in U_Rec.First_Invocation_Construct ..
                      U_Rec.Last_Invocation_Construct
         loop
            Process_Invocation_Construct (IC_Id, Unit_LGN_Id);
         end loop;
      end Process_Invocation_Constructs;

      ---------------------------------
      -- Process_Invocation_Relation --
      ---------------------------------

      procedure Process_Invocation_Relation (IR_Id : Invocation_Relation_Id) is
         pragma Assert (Present (IR_Id));

         IR_Rec : Invocation_Relation_Record renames
                    Invocation_Relations.Table (IR_Id);

         Invoker : constant Invocation_Graph_Node_Id :=
                     Invocation_Graph_Node_Of (IR_Rec.Invoker);
         Target  : constant Invocation_Graph_Node_Id :=
                     Invocation_Graph_Node_Of (IR_Rec.Target);

         IGE_Id : Invocation_Graph_Edge_Id;

      begin
         pragma Assert (Present (Invoker));
         pragma Assert (Present (Target));

         --  Create the invocation graph edge attributes

         Edge_Attributes.Append
           ((Relation => IR_Id,
             Target   => Target));
         IGE_Id := Edge_Attributes.Last;

         --  Add the edge to the list of iterable edges

         IGE_List.Append (Iterable_Edges, IGE_Id);

         --  The edge is owned by the invoker

         IGE_List.Append (Edges_To_Targets (Invoker), IGE_Id);

         --  Update the edge statistics

         Increment_Invocation_Graph_Edge_Count (IR_Rec.Kind);
      end Process_Invocation_Relation;

      ----------------------------------
      -- Process_Invocation_Relations --
      ----------------------------------

      procedure Process_Invocation_Relations (U_Id : Unit_Id) is
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames Units.Table (U_Id);

      begin
         --  Process each invocation relation of the unit

         for IR_Id in U_Rec.First_Invocation_Relation ..
                      U_Rec.Last_Invocation_Relation
         loop
            Process_Invocation_Relation (IR_Id);
         end loop;
      end Process_Invocation_Relations;

      -------------------
      -- Process_Units --
      -------------------

      procedure Process_Units is
      begin
         --  Process all invocation constructs declared in all units. This
         --  creates the corresponding library graph nodes for the units.

         for U_Id in Units.First .. Units.Last loop
            if Requires_Processing (U_Id) then
               Process_Invocation_Constructs (U_Id);
            end if;
         end loop;

         --  Process all invocation relations for all units. This creates the
         --  corresponding library graph edges for the relations.

         for U_Id in Units.First .. Units.Last loop
            if Requires_Processing (U_Id) then
               Process_Invocation_Relations (U_Id);
            end if;
         end loop;
      end Process_Units;

      --------------
      -- Relation --
      --------------

      function Relation
        (IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Relation_Id
      is
      begin
         pragma Assert (Present (IGE_Id));
         return Edge_Attributes.Table (IGE_Id).Relation;
      end Relation;

      ---------------------------
      -- Save_Elaboration_Root --
      ---------------------------

      procedure Save_Elaboration_Root (IGN_Id : Invocation_Graph_Node_Id) is
      begin
         pragma Assert (Present (IGN_Id));

         --  Save the invocation graph node only when it denotes a spec or body
         --  elaboration procedure, and has not been saved yet.

         if Is_Elaboration_Root (IGN_Id)
           and then not Is_Recorded_Elaboration_Root (IGN_Id)
         then
            Set_Is_Recorded_Elaboration_Root (IGN_Id);

            --  Add the elaboration root to the list of iterable elaboration
            --  roots.

            IGN_List.Append (Iterable_Elaboration_Roots, IGN_Id);
         end if;
      end Save_Elaboration_Root;

      -------------------
      -- Set_Construct --
      -------------------

      procedure Set_Construct
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Invocation_Construct_Id)
      is
      begin
         pragma Assert (Present (IGN_Id));
         Node_Attributes.Table (IGN_Id).Construct := Val;
      end Set_Construct;

      --------------------------------------
      -- Set_Is_Recorded_Elaboration_Root --
      --------------------------------------

      procedure Set_Is_Recorded_Elaboration_Root
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Boolean := True)
      is
      begin
         pragma Assert (Present (IGN_Id));

         if Val then
            IGN_Set.Insert (Elaboration_Roots_Set, IGN_Id);
         else
            IGN_Set.Delete (Elaboration_Roots_Set, IGN_Id);
         end if;
      end Set_Is_Recorded_Elaboration_Root;

      ------------------
      -- Set_Lib_Node --
      ------------------

      procedure Set_Lib_Node
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Library_Graph_Node_Id)
      is
      begin
         pragma Assert (Present (IGN_Id));
         Node_Attributes.Table (IGN_Id).Lib_Node := Val;
      end Set_Lib_Node;

      ------------
      -- Target --
      ------------

      function Target
        (IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Graph_Node_Id
      is
      begin
         pragma Assert (Present (IGE_Id));
         return Edge_Attributes.Table (IGE_Id).Target;
      end Target;

      -----------------------------
      -- Total_Elaboration_Roots --
      -----------------------------

      function Total_Elaboration_Roots return Natural is
      begin
         return IGN_Set.Size (Elaboration_Roots_Set);
      end Total_Elaboration_Roots;

      ----------------------------------
      -- Total_Invocation_Graph_Edges --
      ----------------------------------

      function Total_Invocation_Graph_Edges return Natural is
      begin
         return IGE_List.Size (Iterable_Edges);
      end Total_Invocation_Graph_Edges;

      ----------------------------------
      -- Total_Invocation_Graph_Nodes --
      ----------------------------------

      function Total_Invocation_Graph_Nodes return Natural is
      begin
         return IGN_List.Size (Iterable_Nodes);
      end Total_Invocation_Graph_Nodes;
   end Invocation_Graph;

   --------------------------------
   -- Invocation_Graph_Validator --
   --------------------------------

   package body Invocation_Graph_Validator is

      Has_Invalid_Data : Boolean := False;
      --  Flag set when the invocation graph contains invalid data

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Validate_Invocation_Graph_Edge
        (IGE_Id : Invocation_Graph_Edge_Id);
      pragma Inline (Validate_Invocation_Graph_Edge);
      --  ???

      procedure Validate_Invocation_Graph_Edges;
      pragma Inline (Validate_Invocation_Graph_Edges);
      --  ???

      procedure Validate_Invocation_Graph_Node
        (IGN_Id : Invocation_Graph_Node_Id);
      pragma Inline (Validate_Invocation_Graph_Node);
      --  ???

      procedure Validate_Invocation_Graph_Nodes;
      pragma Inline (Validate_Invocation_Graph_Nodes);
      --  ???

      procedure Write_Error (Msg : String);
      pragma Inline (Write_Error);
      --  ???

      -------------------------------
      -- Validate_Invocation_Graph --
      -------------------------------

      procedure Validate_Invocation_Graph is
      begin
         --  Nothing to do when switch -d_V (validate bindo graphs and order)
         --  is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Validate_Invocation_Graph_Nodes;
         Validate_Invocation_Graph_Edges;

         if Has_Invalid_Data then
            raise Invalid_Invocation_Graph;
         end if;
      end Validate_Invocation_Graph;

      ------------------------------------
      -- Validate_Invocation_Graph_Edge --
      ------------------------------------

      procedure Validate_Invocation_Graph_Edge
        (IGE_Id : Invocation_Graph_Edge_Id)
      is
         Msg : constant String := "Validate_Invocation_Graph_Edge";

      begin
         if not Present (IGE_Id) then
            Write_Error (Msg);

            Write_Str ("  emply invocation graph edge");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if not Present (Relation (IGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph edge (IGE_Id_");
            Write_Int (Int (IGE_Id));
            Write_Str (") lacks Relation");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Target (IGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph edge (IGE_Id_");
            Write_Int (Int (IGE_Id));
            Write_Str (") lacks Target");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Invocation_Graph_Edge;

      -------------------------------------
      -- Validate_Invocation_Graph_Edges --
      -------------------------------------

      procedure Validate_Invocation_Graph_Edges is
         IGE_Id : Invocation_Graph_Edge_Id;
         Iter   : IGE_List.Iterator;

      begin
         Iter := Iterate_Invocation_Graph_Edges;
         while IGE_List.Has_Next (Iter) loop
            IGE_List.Next (Iter, IGE_Id);

            Validate_Invocation_Graph_Edge (IGE_Id);
         end loop;
      end Validate_Invocation_Graph_Edges;

      ------------------------------------
      -- Validate_Invocation_Graph_Node --
      ------------------------------------

      procedure Validate_Invocation_Graph_Node
        (IGN_Id : Invocation_Graph_Node_Id)
      is
         Msg : constant String := "Validate_Invocation_Graph_Node";

      begin
         if not Present (IGN_Id) then
            Write_Error (Msg);

            Write_Str ("  emply invocation graph node");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if not Present (Construct (IGN_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph node (IGN_Id_");
            Write_Int (Int (IGN_Id));
            Write_Str (") lacks Construct");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Lib_Node (IGN_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph node (IGN_Id_");
            Write_Int (Int (IGN_Id));
            Write_Str (") lacks Lib_Node");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Invocation_Graph_Node;

      -------------------------------------
      -- Validate_Invocation_Graph_Nodes --
      -------------------------------------

      procedure Validate_Invocation_Graph_Nodes is
         IGN_Id : Invocation_Graph_Node_Id;
         Iter   : IGN_List.Iterator;

      begin
         Iter := Iterate_Invocation_Graph_Nodes;
         while IGN_List.Has_Next (Iter) loop
            IGN_List.Next (Iter, IGN_Id);

            Validate_Invocation_Graph_Node (IGN_Id);
         end loop;
      end Validate_Invocation_Graph_Nodes;

      -----------------
      -- Write_Error --
      -----------------

      procedure Write_Error (Msg : String) is
      begin
         Has_Invalid_Data := True;

         Write_Str ("ERROR: ");
         Write_Str (Msg);
         Write_Eol;
      end Write_Error;
   end Invocation_Graph_Validator;

   -----------------------------
   -- Invocation_Graph_Writer --
   -----------------------------

   package body Invocation_Graph_Writer is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Invocation_Graph_Edge
        (IGE_Id : Invocation_Graph_Edge_Id);
      pragma Inline (Write_Invocation_Graph_Edge);
      --  ???

      procedure Write_Invocation_Graph_Edges (Edges : IGE_List.Instance);
      pragma Inline (Write_Invocation_Graph_Edges);
      --  ???

      procedure Write_Invocation_Graph_Node
        (IGN_Id : Invocation_Graph_Node_Id);
      pragma Inline (Write_Invocation_Graph_Node);
      --  ???

      procedure Write_Invocation_Graph_Nodes;
      pragma Inline (Write_Invocation_Graph_Nodes);
      --  ???

      -----------
      -- Debug --
      -----------

      procedure pige (IGE_Id : Invocation_Graph_Edge_Id)
        renames Write_Invocation_Graph_Edge;
      pragma Unreferenced (pige);

      procedure pign (IGN_Id : Invocation_Graph_Node_Id)
        renames Write_Invocation_Graph_Node;
      pragma Unreferenced (pign);

      ---------------------------------
      -- Write_Invocation_Graph_Edge --
      ---------------------------------

      procedure Write_Invocation_Graph_Edge
        (IGE_Id : Invocation_Graph_Edge_Id)
      is
         Targ : Invocation_Graph_Node_Id;

      begin
         pragma Assert (Present (IGE_Id));

         Write_Str ("    invocation graph edge (IGE_Id_");
         Write_Int (Int (IGE_Id));
         Write_Str (")");
         Write_Eol;

         Write_Str ("      Relation (IR_Id_");
         Write_Int (Int (Relation (IGE_Id)));
         Write_Str (")");
         Write_Eol;

         Targ := Target (IGE_Id);
         pragma Assert (Present (Targ));

         Write_Str  ("      Target (IGN_Id_");
         Write_Int  (Int (Targ));
         Write_Str  (") name = ");
         Write_Name (Name (Targ));
         Write_Eol;

         Write_Eol;
      end Write_Invocation_Graph_Edge;

      ----------------------------------
      -- Write_Invocation_Graph_Edges --
      ----------------------------------

      procedure Write_Invocation_Graph_Edges (Edges : IGE_List.Instance) is
         Iter   : IGE_List.Iterator;
         IGE_Id : Invocation_Graph_Edge_Id;

      begin
         Write_Str ("  Edges_To_Targets");
         Write_Eol;

         Iter := IGE_List.Iterate (Edges);
         while IGE_List.Has_Next (Iter) loop
            IGE_List.Next (Iter, IGE_Id);

            Write_Invocation_Graph_Edge (IGE_Id);
         end loop;
      end Write_Invocation_Graph_Edges;

      ----------------------------
      -- Write_Invocation_Graph --
      ----------------------------

      procedure Write_Invocation_Graph is
      begin
         --  Nothing to do when switch -d_I (output invocation graph) is not in
         --  effect.

         if not Debug_Flag_Underscore_II then
            return;
         end if;

         Write_Str ("Invocation Graph");
         Write_Eol;
         Write_Eol;

         Write_Str ("Invocation graph nodes: ");
         Write_Num (Int (Total_Invocation_Graph_Nodes));
         Write_Eol;

         Write_Str ("Elaboration roots     : ");
         Write_Num (Int (Total_Elaboration_Roots));
         Write_Eol;

         Write_Str ("Invocation graph edges: ");
         Write_Num (Int (Total_Invocation_Graph_Edges));
         Write_Eol;

         for Kind in Invocation_Kind'Range loop
            Write_Str ("  ");
            Write_Num (Int (Invocation_Graph_Edge_Count (Kind)));
            Write_Str (" - ");
            Write_Str (Kind'Img);
            Write_Eol;
         end loop;

         Write_Eol;

         Write_Invocation_Graph_Nodes;

         Write_Str ("Invocation Graph end");
         Write_Eol;

         Write_Eol;
      end Write_Invocation_Graph;

      ---------------------------------
      -- Write_Invocation_Graph_Node --
      ---------------------------------

      procedure Write_Invocation_Graph_Node
        (IGN_Id : Invocation_Graph_Node_Id)
      is
         pragma Assert (Present (IGN_Id));

         Targets : constant IGE_List.Instance := Edges_To_Targets (IGN_Id);
         LGN_Id  : Library_Graph_Node_Id;

      begin
         Write_Str  ("invocation graph node (IGN_Id_");
         Write_Int  (Int (IGN_Id));
         Write_Str  (") name = ");
         Write_Name (Name (IGN_Id));
         Write_Eol;

         Write_Str ("  Construct (IC_Id_");
         Write_Int (Int (Construct (IGN_Id)));
         Write_Str (")");
         Write_Eol;

         LGN_Id := Lib_Node (IGN_Id);
         pragma Assert (Present (LGN_Id));

         Write_Str  ("  Lib_Node (LGN_Id_");
         Write_Int  (Int (LGN_Id));
         Write_Str  (") name = ");
         Write_Name (Name (LGN_Id));
         Write_Eol;

         if IGE_List.Is_Empty (Targets) then
            Write_Eol;
         else
            Write_Invocation_Graph_Edges (Targets);
         end if;
      end Write_Invocation_Graph_Node;

      ----------------------------------
      -- Write_Invocation_Graph_Nodes --
      ----------------------------------

      procedure Write_Invocation_Graph_Nodes is
         IGN_Id : Invocation_Graph_Node_Id;
         Iter   : IGN_List.Iterator;

      begin
         Iter := Iterate_Invocation_Graph_Nodes;
         while IGN_List.Has_Next (Iter) loop
            IGN_List.Next (Iter, IGN_Id);

            Write_Invocation_Graph_Node (IGN_Id);
         end loop;
      end Write_Invocation_Graph_Nodes;
   end Invocation_Graph_Writer;

   -------------------
   -- Library_Graph --
   -------------------

   package body Library_Graph is

      ------------------------
      -- Library graph edge --
      ------------------------

      --  The following type represents a library graph edge

      type Library_Graph_Edge_Record is record
         Kind : Library_Graph_Edge_Kind := No_Edge;
         --  The nature of the library graph edge

         Predecessor : Library_Graph_Node_Id := No_Library_Graph_Node;
         --  A reference to the predecessor library graph node

         Successor : Library_Graph_Node_Id := No_Library_Graph_Node;
         --  A reference to the successor library graph node
      end record;

      ------------------------
      -- Library graph node --
      ------------------------

      --  The following type represents a library graph node

      type Library_Graph_Node_Record is record
         Corresponding_Item : Library_Graph_Node_Id := No_Library_Graph_Node;
         --  A reference to the corresponding spec or body. This attribute is
         --  set as follows:
         --
         --    * If predicate Is_Body_With_Spec is True, the reference denotes
         --      the corresponding spec.
         --
         --    * If predicate Is_Spec_With_Body is True, the reference denotes
         --      the corresponding body.
         --
         --    * Otherwise the attribute remains empty.

         Edges_To_Successors : LGE_List.Instance := LGE_List.Nil;
         --  List of all library graph edges which lead to successor library
         --  graph nodes.

         In_Elaboration_Order : Boolean := False;
         --  Set when this node is chosen for the elaboration order

         Pending_Predecessors : Natural := 0;
         --  Number of pending predecessor library graph nodes that must be
         --  chosen in the elaboration order prior to this node.

         SCC : SCC_Graph_Node_Id := No_SCC_Graph_Node;
         --  Reference to the SCC graph node this node belongs to

         Unit : Unit_Id := No_Unit_Id;
         --  Reference to the associated unit
      end record;

      ----------------
      -- Statistics --
      ----------------

      Library_Graph_Edge_Counts : array (Library_Graph_Edge_Kind) of Natural;

      ---------------------
      -- Data structures --
      ---------------------

      --  The following table stores the attributes of all library graph edges

      package Edge_Attributes is new Table.Table
        (Table_Index_Type     => Library_Graph_Edge_Id,
         Table_Component_Type => Library_Graph_Edge_Record,
         Table_Low_Bound      => First_Library_Graph_Edge,
         Table_Initial        => 500,
         Table_Increment      => 200,
         Table_Name           => "Edge_Attributes");

      Iterable_Edges : LGE_List.Instance := LGE_List.Nil;
      --  The iterable form of all library graph edges

      --  The following table stores the attributes of all library graph nodes

      package Node_Attributes is new Table.Table
        (Table_Index_Type     => Library_Graph_Node_Id,
         Table_Component_Type => Library_Graph_Node_Record,
         Table_Low_Bound      => First_Library_Graph_Node,
         Table_Initial        => 200,
         Table_Increment      => 200,
         Table_Name           => "Node_Attributes");

      Iterable_Nodes : LGN_List.Instance := LGN_List.Nil;
      --  The iterable form of all library graph nodes

      --  The following set stores all recorded library graph edges. It is used
      --  to prevent duplicate edges between the same predecessor and successor
      --  library graph nodes. The set operates with Records instead of Ids
      --  because it is used in cases where tentative edges are tested, but not
      --  inserted in table Edges.

      function Hash
        (LGE_Rec : Library_Graph_Edge_Record) return Bucket_Range_Type;
      --  Obtain the hash value of key LGE_Rec

      function Same_Keys
        (Left  : Library_Graph_Edge_Record;
         Right : Library_Graph_Edge_Record) return Boolean;
      --  Determine whether keys Left and Right are the same

      package RE is new Membership_Set
        (Element_Type => Library_Graph_Edge_Record,
         "="          => Same_Keys,
         Hash         => Hash);

      Recorded_Edges_Set : RE.Instance := RE.Nil;

      --  The following map relates units to nodes

      package UN is new Dynamic_HTable
        (Key_Type              => Unit_Id,
         Value_Type            => Library_Graph_Node_Id,
         No_Value              => No_Library_Graph_Node,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         Destroy_Value         => Destroy,
         "="                   => "=",
         Hash                  => Hash);

      Unit_To_Node_Map : UN.Instance := UN.Nil;

      -----------------------
      -- Local subprograms --
      -----------------------

      function Corresponding_Body (U_Id : Unit_Id) return Unit_Id;
      pragma Inline (Corresponding_Body);
      --  Return the body of a spec unit with id U_Id

      function Corresponding_Spec (U_Id : Unit_Id) return Unit_Id;
      pragma Inline (Corresponding_Spec);
      --  Return the spec of a body unit with id U_Id

      procedure Increment_Library_Graph_Edge_Count
        (Kind : Library_Graph_Edge_Kind);
      pragma Inline (Increment_Library_Graph_Edge_Count);
      --  ???

      function Is_Recorded_Edge
        (LGE_Rec : Library_Graph_Edge_Record) return Boolean;
      pragma Inline (Is_Recorded_Edge);
      --  Determine whether the predecessor and successor of library graph edge
      --  LGE_Rec are already associated by means of another edge.

      function Present (W_Id : With_Id) return Boolean;
      pragma Inline (Present);
      --  Determine whether a with denoted by its id W_Id exists

      procedure Process_Unit (U_Id : Unit_Id);
      pragma Inline (Process_Unit);
      --  Process a unit with id U_Id for graph creation purposes

      procedure Process_Units;
      pragma Inline (Process_Units);
      --  Process all units for graph creation purposes

      procedure Process_With (W_Id : With_Id; Succ : Library_Graph_Node_Id);
      pragma Inline (Process_With);
      --  Process a with denoted by its id W_Id for graph creation purposes.
      --  Succ is the library graph node of the successor that owns the with.

      procedure Process_Withs (U_Id : Unit_Id; Succ : Library_Graph_Node_Id);
      pragma Inline (Process_Withs);
      --  Process all withs of a unit with id U_Id for graph creation purposes.
      --  Succ is the library graph node of the unit.

      function Requires_Processing (W_Id : With_Id) return Boolean;
      pragma Inline (Requires_Processing);
      --  Determine whether a with denoted by its id W_Id needs to be processed
      --  for graph creation purposes.

      procedure Set_Corresponding_Item
        (LGN_Id : Library_Graph_Node_Id;
         Val    : Library_Graph_Node_Id);
      pragma Inline (Set_Corresponding_Item);
      --  Set attribute Corresponding_Item of a library graph node with id
      --  LGN_Id to Val.

      procedure Set_Is_Recorded_Edge
        (LGE_Rec : Library_Graph_Edge_Record;
         Val     : Boolean := True);
      pragma Inline (Set_Is_Recorded_Edge);
      --  Mark library graph edge LGE_Rec as recorded depending on the value of
      --  Val.

      function Unit_Id_Of (Nam : Unit_Name_Type) return Unit_Id;
      pragma Inline (Unit_Id_Of);
      --  Obtain the unit id of a unit with name Nam

      ------------------------------
      -- Build_Library_Graph_Edge --
      ------------------------------

      procedure Build_Library_Graph_Edge
        (Pred : Library_Graph_Node_Id;
         Succ : Library_Graph_Node_Id;
         Kind : Library_Graph_Edge_Kind)
      is
         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));
         pragma Assert (Kind /= No_Edge);

         LGE_Rec : constant Library_Graph_Edge_Record :=
                     (Kind        => Kind,
                      Predecessor => Pred,
                      Successor   => Succ);

         LGE_Id : Library_Graph_Edge_Id;

      begin
         --  No previous library graph edge exists between the predecessor and
         --  successor.

         if not Is_Recorded_Edge (LGE_Rec) then

            --  Mark the edge as recorded. This prevents all further attempts
            --  to link the same predecessor and successor.

            Set_Is_Recorded_Edge (LGE_Rec);

            --  Create the edge attributes

            Edge_Attributes.Append (LGE_Rec);
            LGE_Id := Edge_Attributes.Last;

            --  Add the edge to the list of iterable edges

            LGE_List.Append (Iterable_Edges, LGE_Id);

            --  The edge is owned by the predecessor

            LGE_List.Append (Edges_To_Successors (Pred), LGE_Id);

            --  Update the number of pending predecessors the successor must
            --  wait before it is chosen for the elaboration order.

            Set_Pending_Predecessors (Succ, Pending_Predecessors (Succ) + 1);

            --  Update the edge statistics

            Increment_Library_Graph_Edge_Count (Kind);
         end if;
      end Build_Library_Graph_Edge;

      -------------------------
      -- Build_Library_Graph --
      -------------------------

      procedure Build_Library_Graph is
      begin
         Process_Units;

         Validate_Library_Graph;
         Write_Library_Graph;
      end Build_Library_Graph;

      ------------------------
      -- Corresponding_Body --
      ------------------------

      function Corresponding_Body (U_Id : Unit_Id) return Unit_Id is
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames Units.Table (U_Id);

      begin
         pragma Assert (U_Rec.Utype = Is_Spec);
         return U_Id - 1;
      end Corresponding_Body;

      ------------------------
      -- Corresponding_Item --
      ------------------------

      function Corresponding_Item
        (LGN_Id : Library_Graph_Node_Id) return Library_Graph_Node_Id
      is
      begin
         pragma Assert (Present (LGN_Id));
         return Node_Attributes.Table (LGN_Id).Corresponding_Item;
      end Corresponding_Item;

      ------------------------
      -- Corresponding_Spec --
      ------------------------

      function Corresponding_Spec (U_Id : Unit_Id) return Unit_Id is
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames Units.Table (U_Id);

      begin
         pragma Assert (U_Rec.Utype = Is_Body);
         return U_Id + 1;
      end Corresponding_Spec;

      -------------------------
      -- Edges_To_Successors --
      -------------------------

      function Edges_To_Successors
        (LGN_Id : Library_Graph_Node_Id) return LGE_List.Instance
      is
      begin
         pragma Assert (Present (LGN_Id));
         return Node_Attributes.Table (LGN_Id).Edges_To_Successors;
      end Edges_To_Successors;

      ----------------------------
      -- Finalize_Library_Graph --
      ----------------------------

      procedure Finalize_Library_Graph is
      begin
         LGE_List.Destroy (Iterable_Edges);
         LGN_List.Destroy (Iterable_Nodes);

         RE.Destroy (Recorded_Edges_Set);
         UN.Destroy (Unit_To_Node_Map);
      end Finalize_Library_Graph;

      ----------
      -- Hash --
      ----------

      function Hash
        (LGE_Rec : Library_Graph_Edge_Record) return Bucket_Range_Type
      is
         Pred : constant Library_Graph_Node_Id := LGE_Rec.Predecessor;
         Succ : constant Library_Graph_Node_Id := LGE_Rec.Successor;

      begin
         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));

         --  The hash is obtained in the following manner:
         --
         --    1) The low bits of the predecessor are obtained, then shifted
         --       over to the high bits position.
         --
         --    2) The low bits of the succssor are obtained.
         --
         --  The results from 1) and 2) are or-ed to produce a value within the
         --  range of Bucket_Range_Type.

         return
           ((Bucket_Range_Type (Pred) and Mask) * Half)  --  (1)
               or
            (Bucket_Range_Type (Succ) and Mask);         --  (2)
      end Hash;

      --------------------------
      -- In_Elaboration_Order --
      --------------------------

      function In_Elaboration_Order
        (LGN_Id : Library_Graph_Node_Id) return Boolean
      is
      begin
         pragma Assert (Present (LGN_Id));
         return Node_Attributes.Table (LGN_Id).In_Elaboration_Order;
      end In_Elaboration_Order;

      ----------------------------------------
      -- Increment_Library_Graph_Edge_Count --
      ----------------------------------------

      procedure Increment_Library_Graph_Edge_Count
        (Kind : Library_Graph_Edge_Kind)
      is
         Count : Natural renames Library_Graph_Edge_Counts (Kind);

      begin
         Count := Count + 1;
      end Increment_Library_Graph_Edge_Count;

      ------------------------------
      -- Initialize_Library_Graph --
      ------------------------------

      procedure Initialize_Library_Graph is
      begin
         Iterable_Edges := LGE_List.Create;
         Iterable_Nodes := LGN_List.Create;

         Recorded_Edges_Set := RE.Create (Total_Units);
         Unit_To_Node_Map   := UN.Create (Total_Units);
      end Initialize_Library_Graph;

      ------------------------
      -- Has_Elaborate_Body --
      ------------------------

      function Has_Elaborate_Body
        (LGN_Id : Library_Graph_Node_Id) return Boolean
      is
         pragma Assert (Present (LGN_Id));

         U_Rec : Unit_Record renames Units.Table (Unit (LGN_Id));

      begin
         return U_Rec.Elaborate_Body;
      end Has_Elaborate_Body;

      -------------
      -- Is_Body --
      -------------

      function Is_Body (LGN_Id : Library_Graph_Node_Id) return Boolean is
         pragma Assert (Present (LGN_Id));

         U_Rec : Unit_Record renames Units.Table (Unit (LGN_Id));

      begin
         return U_Rec.Utype = Is_Body or else U_Rec.Utype = Is_Body_Only;
      end Is_Body;

      -----------------------
      -- Is_Body_With_Spec --
      -----------------------

      function Is_Body_With_Spec
        (LGN_Id : Library_Graph_Node_Id) return Boolean
      is
         pragma Assert (Present (LGN_Id));

         U_Rec : Unit_Record renames Units.Table (Unit (LGN_Id));

      begin
         return U_Rec.Utype = Is_Body;
      end Is_Body_With_Spec;

      ----------------------
      -- Is_Internal_Unit --
      ----------------------

      function Is_Internal_Unit
        (LGN_Id : Library_Graph_Node_Id) return Boolean
      is
         pragma Assert (Present (LGN_Id));

         U_Rec : Unit_Record renames Units.Table (Unit (LGN_Id));

      begin
         return U_Rec.Internal;
      end Is_Internal_Unit;

      ------------------------
      -- Is_Predefined_Unit --
      ------------------------

      function Is_Predefined_Unit
        (LGN_Id : Library_Graph_Node_Id) return Boolean
      is
         pragma Assert (Present (LGN_Id));

         U_Rec : Unit_Record renames Units.Table (Unit (LGN_Id));

      begin
         return U_Rec.Predefined;
      end Is_Predefined_Unit;

      ---------------------------
      -- Is_Preelaborated_Unit --
      ---------------------------

      function Is_Preelaborated_Unit
        (LGN_Id : Library_Graph_Node_Id) return Boolean
      is
         Spec : constant Library_Graph_Node_Id := Proper_Spec (LGN_Id);
         pragma Assert (Present (Spec));

         U_Rec : Unit_Record renames Units.Table (Unit (Spec));

      begin
         return U_Rec.Preelab or else U_Rec.Pure;
      end Is_Preelaborated_Unit;

      ----------------------
      -- Is_Recorded_Edge --
      ----------------------

      function Is_Recorded_Edge
        (LGE_Rec : Library_Graph_Edge_Record) return Boolean
      is
      begin
         return RE.Contains (Recorded_Edges_Set, LGE_Rec);
      end Is_Recorded_Edge;

      -------------
      -- Is_Spec --
      -------------

      function Is_Spec (LGN_Id : Library_Graph_Node_Id) return Boolean is
         pragma Assert (Present (LGN_Id));

         U_Rec : Unit_Record renames Units.Table (Unit (LGN_Id));

      begin
         return U_Rec.Utype = Is_Spec or else U_Rec.Utype = Is_Spec_Only;
      end Is_Spec;

      -----------------------
      -- Is_Spec_With_Body --
      -----------------------

      function Is_Spec_With_Body
        (LGN_Id : Library_Graph_Node_Id) return Boolean
      is
         pragma Assert (Present (LGN_Id));

         U_Rec : Unit_Record renames Units.Table (Unit (LGN_Id));

      begin
         return U_Rec.Utype = Is_Spec;
      end Is_Spec_With_Body;

      ---------------------------------
      -- Iterate_Library_Graph_Edges --
      ---------------------------------

      function Iterate_Library_Graph_Edges return LGE_List.Iterator is
      begin
         return LGE_List.Iterate (Iterable_Edges);
      end Iterate_Library_Graph_Edges;

      ---------------------------------
      -- Iterate_Library_Graph_Nodes --
      ---------------------------------

      function Iterate_Library_Graph_Nodes return LGN_List.Iterator is
      begin
         return LGN_List.Iterate (Iterable_Nodes);
      end Iterate_Library_Graph_Nodes;

      ----------
      -- Kind --
      ----------

      function Kind
        (LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Edge_Kind
      is
      begin
         pragma Assert (Present (LGE_Id));
         return Edge_Attributes.Table (LGE_Id).Kind;
      end Kind;

      ------------------------------
      -- Library_Graph_Edge_Count --
      ------------------------------

      function Library_Graph_Edge_Count
        (Kind : Library_Graph_Edge_Kind) return Natural
      is
      begin
         return Library_Graph_Edge_Counts (Kind);
      end Library_Graph_Edge_Count;

      ---------------------------
      -- Library_Graph_Node_Of --
      ---------------------------

      function Library_Graph_Node_Of
        (U_Id : Unit_Id) return Library_Graph_Node_Id
      is
         LGN_Id : Library_Graph_Node_Id;

      begin
         pragma Assert (Present (U_Id));

         LGN_Id := UN.Get (Unit_To_Node_Map, U_Id);

         --  The unit lacks a library graph node. This indicates that the unit
         --  is encountered for the first time.

         if not Present (LGN_Id) then

            --  Create the node attributes

            Node_Attributes.Append
              ((Corresponding_Item    => No_Library_Graph_Node,
                Edges_To_Successors   => LGE_List.Create,
                In_Elaboration_Order  => False,
                Pending_Predecessors  => 0,
                SCC                   => No_SCC_Graph_Node,
                Unit                  => U_Id));
            LGN_Id := Node_Attributes.Last;

            --  Add the node to the list of iterable nodes

            LGN_List.Append (Iterable_Nodes, LGN_Id);

            --  Associate the node with the unit

            UN.Put (Unit_To_Node_Map, U_Id, LGN_Id);
         end if;

         return LGN_Id;
      end Library_Graph_Node_Of;

      ----------
      -- Name --
      ----------

      function Name (LGN_Id : Library_Graph_Node_Id) return Unit_Name_Type is
      begin
         pragma Assert (Present (LGN_Id));
         return Name (Unit (LGN_Id));
      end Name;

      --------------------------
      -- Pending_Predecessors --
      --------------------------

      function Pending_Predecessors
        (LGN_Id : Library_Graph_Node_Id) return Natural
      is
      begin
         pragma Assert (Present (LGN_Id));
         return Node_Attributes.Table (LGN_Id).Pending_Predecessors;
      end Pending_Predecessors;

      -----------------
      -- Predecessor --
      -----------------

      function Predecessor
        (LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Node_Id
      is
      begin
         pragma Assert (Present (LGE_Id));
         return Edge_Attributes.Table (LGE_Id).Predecessor;
      end Predecessor;

      -------------
      -- Present --
      -------------

      function Present (LGE_Id : Library_Graph_Edge_Id) return Boolean is
      begin
         return LGE_Id /= No_Library_Graph_Edge;
      end Present;

      -------------
      -- Present --
      -------------

      function Present (LGN_Id : Library_Graph_Node_Id) return Boolean is
      begin
         return LGN_Id /= No_Library_Graph_Node;
      end Present;

      -------------
      -- Present --
      -------------

      function Present (W_Id : With_Id) return Boolean is
      begin
         return W_Id /= No_With_Id;
      end Present;

      ------------------
      -- Process_Unit --
      ------------------

      procedure Process_Unit (U_Id : Unit_Id) is
         pragma Assert (Present (U_Id));

         LGN_Id : constant Library_Graph_Node_Id :=
                    Library_Graph_Node_Of (U_Id);

         Aux_LGN_Id : Library_Graph_Node_Id;

      begin
         Process_Withs (U_Id, LGN_Id);

         --  The unit denotes a body that completes a previous spec. Link the
         --  spec and body.

         if Is_Body_With_Spec (LGN_Id) then
            Aux_LGN_Id := Library_Graph_Node_Of (Corresponding_Spec (U_Id));
            pragma Assert (Present (Aux_LGN_Id));

            Set_Corresponding_Item (LGN_Id, Aux_LGN_Id);

            --  Add a library graph edge between the predecessor spec and the
            --  successor body.

            Build_Library_Graph_Edge
              (Pred => Aux_LGN_Id,
               Succ => LGN_Id,
               Kind => Spec_Before_Body_Edge);

         --  The unit denotes a spec with a completing body. Link the spec and
         --  body.

         elsif Is_Spec_With_Body (LGN_Id) then
            Aux_LGN_Id := Library_Graph_Node_Of (Corresponding_Body (U_Id));
            pragma Assert (Present (Aux_LGN_Id));

            Set_Corresponding_Item (LGN_Id, Aux_LGN_Id);
         end if;
      end Process_Unit;

      -------------------
      -- Process_Units --
      -------------------

      procedure Process_Units is
      begin
         for U_Id in Units.First .. Units.Last loop
            if Requires_Processing (U_Id) then
               Process_Unit (U_Id);
            end if;
         end loop;
      end Process_Units;

      ------------------
      -- Process_With --
      ------------------

      procedure Process_With (W_Id : With_Id; Succ : Library_Graph_Node_Id) is
         pragma Assert (Present (W_Id));

         Aux_LGN_Id : Library_Graph_Node_Id;
         Kind       : Library_Graph_Edge_Kind;
         Withed_Rec : With_Record renames Withs.Table (W_Id);

         Withed_U_Id   : constant Unit_Id               :=
                           Unit_Id_Of (Withed_Rec.Uname);
         Withed_LGN_Id : constant Library_Graph_Node_Id :=
                           Library_Graph_Node_Of (Withed_U_Id);

      begin
         --  The with comes with pragma Elaborate

         if Withed_Rec.Elaborate then
            Kind := Elaborate_Edge;

            --  The withed unit is a spec with a completing body

            if Is_Spec_With_Body (Withed_LGN_Id) then
               Aux_LGN_Id :=
                 Library_Graph_Node_Of (Corresponding_Body (Withed_U_Id));
               pragma Assert (Present (Aux_LGN_Id));

               --  Add a library graph edge between the body of the withed
               --  predecessor and the withing successor.

               Build_Library_Graph_Edge
                 (Pred => Aux_LGN_Id,
                  Succ => Succ,
                  Kind => Kind);
            end if;

         --  The with comes with pragma Elaborate_All

         elsif Withed_Rec.Elaborate_All then
            Kind := Elaborate_All_Edge;

         --  Otherwise the with is a regular with

         else
            Kind := With_Edge;
         end if;

         --  Add a library graph edge between the withed predecessor unit and
         --  the withing successor.

         Build_Library_Graph_Edge
           (Pred => Withed_LGN_Id,
            Succ => Succ,
            Kind => Kind);
      end Process_With;

      -------------------
      -- Process_Withs --
      -------------------

      procedure Process_Withs (U_Id : Unit_Id; Succ : Library_Graph_Node_Id) is
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames Units.Table (U_Id);

      begin
         for W_Id in U_Rec.First_With .. U_Rec.Last_With loop
            if Requires_Processing (W_Id) then
               Process_With (W_Id, Succ);
            end if;
         end loop;
      end Process_Withs;

      -----------------
      -- Proper_Body --
      -----------------

      function Proper_Body
        (LGN_Id : Library_Graph_Node_Id) return Library_Graph_Node_Id
      is
      begin
         --  When the library graph node denotes a spec with a completing body,
         --  return the body.

         if Is_Spec_With_Body (LGN_Id) then
            return Corresponding_Item (LGN_Id);

         --  Otherwise the node must denote a body

         else
            pragma Assert (Is_Body (LGN_Id));
            return LGN_Id;
         end if;
      end Proper_Body;

      -----------------
      -- Proper_Spec --
      -----------------

      function Proper_Spec
        (LGN_Id : Library_Graph_Node_Id) return Library_Graph_Node_Id
      is
      begin
         --  When the library graph node denotes a body that completes a spec,
         --  return the spec.

         if Is_Body_With_Spec (LGN_Id) then
            return Corresponding_Item (LGN_Id);

         --  Otherwise the node must denote a spec

         else
            pragma Assert (Is_Spec (LGN_Id));
            return LGN_Id;
         end if;
      end Proper_Spec;

      ----------------------------------
      -- Remove_Body_Before_Spec_Edge --
      ----------------------------------

      procedure Remove_Body_Before_Spec_Edge
        (LGE_Id : Library_Graph_Edge_Id;
         Pred   : Library_Graph_Node_Id;
         Succ   : Library_Graph_Node_Id)
      is
         pragma Assert (Present (LGE_Id));
         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));

         Edges_To_Succ : constant LGE_List.Instance :=
                           Edges_To_Successors (Pred);
         Last_LGE_Id : Library_Graph_Edge_Id;

      begin
         --  The library graph edge is owned by the predecessor. Remove it from
         --  its list of edges to successors.

         Last_LGE_Id := LGE_List.Last (Edges_To_Succ);
         pragma Assert (Present (Last_LGE_Id));
         pragma Assert (Last_LGE_Id = LGE_Id);

         LGE_List.Delete_Last (Edges_To_Succ);

         --  Mark the edge as no longer recorded. This allows further attempts
         --  to link the same predecessor and successor.

         Set_Is_Recorded_Edge (Edge_Attributes.Table (LGE_Id), False);

         --  Update the number of pending predecessors the successor must wait
         --  before it is chosen for the elaboration order.

         Set_Pending_Predecessors (Succ, Pending_Predecessors (Succ) - 1);
      end Remove_Body_Before_Spec_Edge;

      -----------------------------------
      -- Remove_Body_Before_Spec_Edges --
      -----------------------------------

      procedure Remove_Body_Before_Spec_Edges is
         procedure Compact_Edge_Attributes;
         pragma Inline (Compact_Edge_Attributes);
         --  Compact table Edge_Attribute by removing Body_Before_Spec library
         --  graph edges.

         procedure Compact_Iterable_Edges;
         pragma Inline (Compact_Iterable_Edges);
         --  Compact list Iterable_Edges by removing Body_Before_Spec library
         --  graph edges.

         -----------------------------
         -- Compact_Edge_Attributes --
         -----------------------------

         procedure Compact_Edge_Attributes is
         begin
            for LGE_Id in reverse Edge_Attributes.First ..
                                  Edge_Attributes.Last
            loop
               --  The current library graph edge is a Body_Before_Spec, remove
               --  it.

               if Kind (LGE_Id) = Body_Before_Spec_Edge then
                  Edge_Attributes.Decrement_Last;

               --  Otherwise the edge must remain in the table. Such an edge
               --  ends the "run" of Body_Before_Spec edges.

               else
                  exit;
               end if;
            end loop;
         end Compact_Edge_Attributes;

         ----------------------------
         -- Compact_Iterable_Edges --
         ----------------------------

         procedure Compact_Iterable_Edges is
            LGE_Id : Library_Graph_Edge_Id;

         begin
            while not LGE_List.Is_Empty (Iterable_Edges) loop
               LGE_Id := LGE_List.Last (Iterable_Edges);

               --  The current library graph edge is a Body_Before_Spec, remove
               --  it.

               if Kind (LGE_Id) = Body_Before_Spec_Edge then
                  LGE_List.Delete_Last (Iterable_Edges);

               --  Otherwise the edge must remain in the list. Such an edge
               --  ends the "run" of Body_Before_Spec edges.

               else
                  exit;
               end if;
            end loop;
         end Compact_Iterable_Edges;

      --  Start of processing for Remove_Body_Before_Spec_Edges

      begin
         --  Body_Before_Spec library graph edges are added after the creation
         --  of the library graph, causing them to accumulate towards the end
         --  of data structures Edge_Attributes and Iterable_Edges.

         Compact_Edge_Attributes;
         Compact_Iterable_Edges;
      end Remove_Body_Before_Spec_Edges;

      -------------------------
      -- Requires_Processing --
      -------------------------

      function Requires_Processing (W_Id : With_Id) return Boolean is
         pragma Assert (Present (W_Id));

         Withed_Rec  : With_Record renames Withs.Table (W_Id);
         Withed_U_Id : constant Unit_Id := Unit_Id_Of (Withed_Rec.Uname);

      begin
         --  Nothing to do for a unit which does not exist any more

         if not Present (Withed_U_Id) then
            return False;

         --  Nothing to do for a unit which does not require any processing

         elsif not Requires_Processing (Withed_U_Id) then
            return False;

         --  Nothing to do for a limited with

         elsif Withed_Rec.Limited_With then
            return False;
         end if;

         return True;
      end Requires_Processing;

      ---------------
      -- Same_Keys --
      ---------------

      function Same_Keys
        (Left  : Library_Graph_Edge_Record;
         Right : Library_Graph_Edge_Record) return Boolean
      is
      begin
         pragma Assert (Present (Left.Predecessor));
         pragma Assert (Present (Left.Successor));
         pragma Assert (Present (Right.Predecessor));
         pragma Assert (Present (Right.Successor));

         --  Two library graph edges are the same when contain have the same
         --  successors and predecessors. Note that attribute Kind does not
         --  play a role here.

         return
           Left.Predecessor = Right.Predecessor
             and then
           Left.Successor   = Right.Successor;
      end Same_Keys;

      ---------
      -- SCC --
      ---------

      function SCC (LGN_Id : Library_Graph_Node_Id) return SCC_Graph_Node_Id is
      begin
         pragma Assert (Present (LGN_Id));
         return Node_Attributes.Table (LGN_Id).SCC;
      end SCC;

      ----------------------------
      -- Set_Corresponding_Item --
      ----------------------------

      procedure Set_Corresponding_Item
        (LGN_Id : Library_Graph_Node_Id;
         Val    : Library_Graph_Node_Id)
      is
      begin
         pragma Assert (Present (LGN_Id));
         Node_Attributes.Table (LGN_Id).Corresponding_Item := Val;
      end Set_Corresponding_Item;

      --------------------------
      -- Set_Is_Recorded_Edge --
      --------------------------

      procedure Set_Is_Recorded_Edge
        (LGE_Rec : Library_Graph_Edge_Record;
         Val     : Boolean := True)
      is
      begin
         if Val then
            RE.Insert (Recorded_Edges_Set, LGE_Rec);
         else
            RE.Delete (Recorded_Edges_Set, LGE_Rec);
         end if;
      end Set_Is_Recorded_Edge;

      ------------------------------
      -- Set_In_Elaboration_Order --
      ------------------------------

      procedure Set_In_Elaboration_Order
        (LGN_Id : Library_Graph_Node_Id;
         Val    : Boolean := True)
      is
      begin
         pragma Assert (Present (LGN_Id));
         Node_Attributes.Table (LGN_Id).In_Elaboration_Order := Val;
      end Set_In_Elaboration_Order;

      ------------------------------
      -- Set_Pending_Predecessors --
      ------------------------------

      procedure Set_Pending_Predecessors
        (LGN_Id : Library_Graph_Node_Id;
         Val    : Natural)
      is
      begin
         pragma Assert (Present (LGN_Id));
         Node_Attributes.Table (LGN_Id).Pending_Predecessors := Val;
      end Set_Pending_Predecessors;

      -------------
      -- Set_SCC --
      -------------

      procedure Set_SCC
        (LGN_Id : Library_Graph_Node_Id;
         Val    : SCC_Graph_Node_Id)
      is
      begin
         pragma Assert (Present (LGN_Id));
         Node_Attributes.Table (LGN_Id).SCC := Val;
      end Set_SCC;

      ---------------
      -- Successor --
      ---------------

      function Successor
        (LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Node_Id
      is
      begin
         pragma Assert (Present (LGE_Id));
         return Edge_Attributes.Table (LGE_Id).Successor;
      end Successor;

      -------------------------------
      -- Total_Library_Graph_Edges --
      -------------------------------

      function Total_Library_Graph_Edges return Natural is
      begin
         return LGE_List.Size (Iterable_Edges);
      end Total_Library_Graph_Edges;

      -------------------------------
      -- Total_Library_Graph_Nodes --
      -------------------------------

      function Total_Library_Graph_Nodes return Natural is
      begin
         return LGN_List.Size (Iterable_Nodes);
      end Total_Library_Graph_Nodes;

      ----------
      -- Unit --
      ----------

      function Unit (LGN_Id : Library_Graph_Node_Id) return Unit_Id is
      begin
         pragma Assert (Present (LGN_Id));
         return Node_Attributes.Table (LGN_Id).Unit;
      end Unit;

      ----------------
      -- Unit_Id_Of --
      ----------------

      function Unit_Id_Of (Nam : Unit_Name_Type) return Unit_Id is
      begin
         return Unit_Id (Get_Name_Table_Int (Nam));
      end Unit_Id_Of;
   end Library_Graph;

   -----------------------------
   -- Library_Graph_Augmentor --
   -----------------------------

   package body Library_Graph_Augmentor is

      ----------------
      -- Statistics --
      ----------------

      Longest_Path_Len : Natural := 0;
      --  The length of the longest path found during the traversal of the
      --  invocation graph.

      Total_Visited_Nodes : Natural := 0;
      --  The number of visited invocation graph nodes during the augmentation
      --  process.

      ---------------------
      -- Data structures --
      ---------------------

      Visited_Set : IGN_Set.Instance := IGN_Set.Nil;
      --  The set of all visited invocation graph nodes

      -----------------------
      -- Local subprograms --
      -----------------------

      function Is_Visited (IGN_Id : Invocation_Graph_Node_Id) return Boolean;
      pragma Inline (Is_Visited);
      --  Determine whether an invocation graph node with id IGN_Id has already
      --  been visited during a traversal.

      procedure Set_Is_Visited
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Boolean := True);
      pragma Inline (Set_Is_Visited);
      --  Mark invocation graph node with id IGN_Id as visited depending on Val

      procedure Visit_Elaboration_Root (IGN_Id : Invocation_Graph_Node_Id);
      pragma Inline (Visit_Elaboration_Root);
      --  Start a DFS traversal from an invocation graph node with id IGN_Id

      procedure Visit_Elaboration_Roots;
      pragma Inline (Visit_Elaboration_Roots);
      --  Start a DFS traversal of each elaboration root of the invocation
      --  graph.

      procedure Visit_Node
        (Curr_IGN_Id : Invocation_Graph_Node_Id;
         Last_LGN_Id : Library_Graph_Node_Id;
         Elab_LGN_Id : Library_Graph_Node_Id;
         Path_Len    : Natural);
      pragma Inline (Visit_Node);
      --  Perform a DFS traversal starting from an invocation graph node with
      --  id Curr_IGN_Id in order to detect transitions from one library graph
      --  node to another. Last_LGN_Id denotes the library graph node of the
      --  predecessor invocation graph node that reached IGN_Id. Elab_LGN_Id
      --  is the library graph node of the elaboration root which started the
      --  DFS traversal. Path_Len is the length of the path traversed so far.

      ---------------------------
      -- Augment_Library_Graph --
      ---------------------------

      procedure Augment_Library_Graph is
      begin
         Visited_Set := IGN_Set.Create (Total_Units);

         --  Kirtchev ??? comment

         Visit_Elaboration_Roots;

         IGN_Set.Destroy (Visited_Set);

         --  Kirtchev ??? comment

         if Debug_Flag_Underscore_LL then
            Write_Str ("Library Graph Augmentation");
            Write_Eol;
            Write_Eol;

            Write_Str ("Invocation graph nodes visited: ");
            Write_Num (Int (Total_Visited_Nodes));
            Write_Eol;

            Write_Str ("Longest path length           : ");
            Write_Num (Int (Longest_Path_Len));
            Write_Eol;
            Write_Eol;

            Write_Str ("Library Graph Augmentation end");
            Write_Eol;
            Write_Eol;
         end if;

         Write_Library_Graph;
      end Augment_Library_Graph;

      ----------------
      -- Is_Visited --
      ----------------

      function Is_Visited (IGN_Id : Invocation_Graph_Node_Id) return Boolean is
      begin
         pragma Assert (Present (IGN_Id));
         return IGN_Set.Contains (Visited_Set, IGN_Id);
      end Is_Visited;

      --------------------
      -- Set_Is_Visited --
      --------------------

      procedure Set_Is_Visited
        (IGN_Id : Invocation_Graph_Node_Id;
         Val    : Boolean := True)
      is
      begin
         if Val then
            IGN_Set.Insert (Visited_Set, IGN_Id);
         else
            IGN_Set.Delete (Visited_Set, IGN_Id);
         end if;
      end Set_Is_Visited;

      ----------------------------
      -- Visit_Elaboration_Root --
      ----------------------------

      procedure Visit_Elaboration_Root (IGN_Id : Invocation_Graph_Node_Id) is
         pragma Assert (Present (IGN_Id));

         LGN_Id : constant Library_Graph_Node_Id := Lib_Node (IGN_Id);

      begin
         --  Reset the visited set for each new elaboration root because
         --  different roots may traverse the same paths.

         IGN_Set.Reset (Visited_Set);

         Visit_Node
           (Curr_IGN_Id => IGN_Id,
            Last_LGN_Id => LGN_Id,
            Elab_LGN_Id => LGN_Id,
            Path_Len    => 0);
      end Visit_Elaboration_Root;

      -----------------------------
      -- Visit_Elaboration_Roots --
      -----------------------------

      procedure Visit_Elaboration_Roots is
         IGN_Id : Invocation_Graph_Node_Id;
         Iter   : IGN_List.Iterator;

      begin
         Iter := Iterate_Elaboration_Roots;
         while IGN_List.Has_Next (Iter) loop
            IGN_List.Next (Iter, IGN_Id);

            Visit_Elaboration_Root (IGN_Id);
         end loop;
      end Visit_Elaboration_Roots;

      ----------------
      -- Visit_Node --
      ----------------

      procedure Visit_Node
        (Curr_IGN_Id : Invocation_Graph_Node_Id;
         Last_LGN_Id : Library_Graph_Node_Id;
         Elab_LGN_Id : Library_Graph_Node_Id;
         Path_Len    : Natural)
      is
         New_Path_Len : constant Natural := Path_Len + 1;

         Curr_LGN_Id : Library_Graph_Node_Id;
         IGE_Id      : Invocation_Graph_Edge_Id;
         Iter        : IGE_List.Iterator;

      begin
         pragma Assert (Present (Curr_IGN_Id));
         pragma Assert (Present (Last_LGN_Id));
         pragma Assert (Present (Elab_LGN_Id));

         --  Nothing to do when the current invocation graph node has already
         --  been visited.

         if Is_Visited (Curr_IGN_Id) then
            return;
         end if;

         --  Mark the current invocation graph node as visited

         Set_Is_Visited (Curr_IGN_Id);

         --  Update the statictics

         Longest_Path_Len    := Natural'Max (Longest_Path_Len, New_Path_Len);
         Total_Visited_Nodes := Total_Visited_Nodes + 1;

         --  The library graph node of the current invocation graph node
         --  differs from that of the previous invocation graph node. This
         --  indicates that elaboration is transitioning from one unit to
         --  another. Add a library graph edge to capture this dependency.

         Curr_LGN_Id := Lib_Node (Curr_IGN_Id);

         if Curr_LGN_Id /= Last_LGN_Id then
            Build_Library_Graph_Edge
              (Pred => Curr_LGN_Id,
               Succ => Elab_LGN_Id,
               Kind => Invocation_Edge);
         end if;

         --  Extend the DFS traversal to all targets of the invocation graph
         --  node.

         Iter := IGE_List.Iterate (Edges_To_Targets (Curr_IGN_Id));
         while IGE_List.Has_Next (Iter) loop
            IGE_List.Next (Iter, IGE_Id);

            Visit_Node
              (Curr_IGN_Id => Target (IGE_Id),
               Last_LGN_Id => Curr_LGN_Id,
               Elab_LGN_Id => Elab_LGN_Id,
               Path_Len    => New_Path_Len);
         end loop;
      end Visit_Node;
   end Library_Graph_Augmentor;

   -----------------------------
   -- Library_Graph_Validator --
   -----------------------------

   package body Library_Graph_Validator is

      Has_Invalid_Data : Boolean := False;
      --  Flag set when the library graph contains invalid data

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Validate_Library_Graph_Edge (LGE_Id : Library_Graph_Edge_Id);
      pragma Inline (Validate_Library_Graph_Edge);
      --  Verify a library graph edge with id LGE_Id

      procedure Validate_Library_Graph_Edges;
      pragma Inline (Validate_Library_Graph_Edges);
      --  Verify all library graph edges

      procedure Validate_Library_Graph_Node (LGN_Id : Library_Graph_Node_Id);
      pragma Inline (Validate_Library_Graph_Node);
      --  Verify a library graph node with id LGN_Id

      procedure Validate_Library_Graph_Nodes;
      pragma Inline (Validate_Library_Graph_Nodes);
      --  Verify all library graph nodes

      procedure Write_Error (Msg : String);
      pragma Inline (Write_Error);
      --  ???

      ----------------------------
      -- Validate_Library_Graph --
      ----------------------------

      procedure Validate_Library_Graph is
      begin
         --  Nothing to do when switch -d_V (validate bindo graphs and order)
         --  is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Validate_Library_Graph_Nodes;
         Validate_Library_Graph_Edges;

         if Has_Invalid_Data then
            raise Invalid_Library_Graph;
         end if;
      end Validate_Library_Graph;

      ---------------------------------
      -- Validate_Library_Graph_Edge --
      ---------------------------------

      procedure Validate_Library_Graph_Edge (LGE_Id : Library_Graph_Edge_Id) is
         Msg : constant String := "Validate_Library_Graph_Edge";

      begin
         if not Present (LGE_Id) then
            Write_Error (Msg);

            Write_Str ("  emply library graph edge");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if Kind (LGE_Id) = No_Edge then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") is not a valid edge");
            Write_Eol;
            Write_Eol;

         elsif Kind (LGE_Id) = Body_Before_Spec_Edge then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") is a Body_Before_Spec edge");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Predecessor (LGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") lacks Predecessor");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Successor (LGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") lacks Successor");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Library_Graph_Edge;

      ----------------------------------
      -- Validate_Library_Graph_Edges --
      ----------------------------------

      procedure Validate_Library_Graph_Edges is
         Iter   : LGE_List.Iterator;
         LGE_Id : Library_Graph_Edge_Id;

      begin
         Iter := Iterate_Library_Graph_Edges;
         while LGE_List.Has_Next (Iter) loop
            LGE_List.Next (Iter, LGE_Id);

            Validate_Library_Graph_Edge (LGE_Id);
         end loop;
      end Validate_Library_Graph_Edges;

      ---------------------------------
      -- Validate_Library_Graph_Node --
      ---------------------------------

      procedure Validate_Library_Graph_Node (LGN_Id : Library_Graph_Node_Id) is
         Msg : constant String := "Validate_Library_Graph_Node";

      begin
         if not Present (LGN_Id) then
            Write_Error (Msg);

            Write_Str ("  empty library graph node");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if (Is_Body_With_Spec (LGN_Id) or else Is_Spec_With_Body (LGN_Id))
           and then not Present (Corresponding_Item (LGN_Id))
         then
            Write_Error (Msg);

            Write_Str ("  library graph node (LGN_Id_");
            Write_Int (Int (LGN_Id));
            Write_Str (") lacks Corresponding_Item");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Unit (LGN_Id)) then
            Write_Error (Msg);

            Write_Str ("  library graph node (LGN_Id_");
            Write_Int (Int (LGN_Id));
            Write_Str (") lacks Unit");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Library_Graph_Node;

      ----------------------------------
      -- Validate_Library_Graph_Nodes --
      ----------------------------------

      procedure Validate_Library_Graph_Nodes is
         Iter   : LGN_List.Iterator;
         LGN_Id : Library_Graph_Node_Id;

      begin
         Iter := Iterate_Library_Graph_Nodes;
         while LGN_List.Has_Next (Iter) loop
            LGN_List.Next (Iter, LGN_Id);

            Validate_Library_Graph_Node (LGN_Id);
         end loop;
      end Validate_Library_Graph_Nodes;

      -----------------
      -- Write_Error --
      -----------------

      procedure Write_Error (Msg : String) is
      begin
         Has_Invalid_Data := True;

         Write_Str ("ERROR: ");
         Write_Str (Msg);
         Write_Eol;
      end Write_Error;
   end Library_Graph_Validator;

   --------------------------
   -- Library_Graph_Writer --
   --------------------------

   package body Library_Graph_Writer is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Library_Graph_Edge (LGE_Id : Library_Graph_Edge_Id);
      pragma Inline (Write_Library_Graph_Edge);
      --  Write out the data of a library graph edge denoted by its id LGE_Id
      --  to standard output.

      procedure Write_Library_Graph_Edges (Edges : LGE_List.Instance);
      pragma Inline (Write_Library_Graph_Edges);
      --  Write out the data of library graph edges Edges to standard output

      procedure Write_Library_Graph_Node (LGN_Id : Library_Graph_Node_Id);
      pragma Inline (Write_Library_Graph_Node);
      --  Write out the data of a library graph node denoted by its id LGN_Id
      --  to standard output.

      procedure Write_Library_Graph_Nodes;
      pragma Inline (Write_Library_Graph_Nodes);
      --  ???

      -----------
      -- Debug --
      -----------

      procedure plge (LGE_Id : Library_Graph_Edge_Id)
        renames Write_Library_Graph_Edge;
      pragma Unreferenced (plge);

      procedure plgn (LGN_Id : Library_Graph_Node_Id)
        renames Write_Library_Graph_Node;
      pragma Unreferenced (plgn);

      ------------------------------
      -- Write_Library_Graph_Edge --
      ------------------------------

      procedure Write_Library_Graph_Edge (LGE_Id : Library_Graph_Edge_Id) is
         pragma Assert (Present (LGE_Id));

         Pred : constant Library_Graph_Node_Id := Predecessor (LGE_Id);
         Succ : constant Library_Graph_Node_Id := Successor   (LGE_Id);

      begin
         Write_Str ("    library graph edge (LGE_Id_");
         Write_Int (Int (LGE_Id));
         Write_Str (")");
         Write_Eol;

         Write_Str ("      Kind = ");
         Write_Str (Kind (LGE_Id)'Img);
         Write_Eol;

         pragma Assert (Present (Pred));

         Write_Str  ("      Predecessor (LGN_Id_");
         Write_Int  (Int (Pred));
         Write_Str  (") name = ");
         Write_Name (Name (Pred));
         Write_Eol;

         pragma Assert (Present (Succ));

         Write_Str  ("      Successor (LGN_Id_");
         Write_Int  (Int (Succ));
         Write_Str  (") name = ");
         Write_Name (Name (Succ));
         Write_Eol;

         Write_Eol;
      end Write_Library_Graph_Edge;

      -------------------------------
      -- Write_Library_Graph_Edges --
      -------------------------------

      procedure Write_Library_Graph_Edges (Edges : LGE_List.Instance) is
         Iter   : LGE_List.Iterator;
         LGE_Id : Library_Graph_Edge_Id;

      begin
         Write_Str ("  Edges_To_Successors");
         Write_Eol;

         Iter := LGE_List.Iterate (Edges);
         while LGE_List.Has_Next (Iter) loop
            LGE_List.Next (Iter, LGE_Id);

            Write_Library_Graph_Edge (LGE_Id);
         end loop;
      end Write_Library_Graph_Edges;

      -------------------------
      -- Write_Library_Graph --
      -------------------------

      procedure Write_Library_Graph is
      begin
         --  Nothing to do when switch -d_L (output library item graph) is not
         --  in effect.

         if not Debug_Flag_Underscore_LL then
            return;
         end if;

         Write_Str ("Library Graph");
         Write_Eol;
         Write_Eol;

         Write_Str ("Library graph nodes: ");
         Write_Num (Int (Total_Library_Graph_Nodes));
         Write_Eol;

         Write_Str ("Library graph edges: ");
         Write_Num (Int (Total_Library_Graph_Edges));
         Write_Eol;

         for Kind in Library_Graph_Edge_Kind'Range loop
            Write_Str ("  ");
            Write_Num (Int (Library_Graph_Edge_Count (Kind)));
            Write_Str (" - ");
            Write_Str (Kind'Img);
            Write_Eol;
         end loop;

         Write_Eol;

         Write_Library_Graph_Nodes;

         Write_Str ("Library Graph end");
         Write_Eol;

         Write_Eol;
      end Write_Library_Graph;

      ------------------------------
      -- Write_Library_Graph_Node --
      ------------------------------

      procedure Write_Library_Graph_Node (LGN_Id : Library_Graph_Node_Id) is
         pragma Assert (Present (LGN_Id));

         Item  : constant Library_Graph_Node_Id := Corresponding_Item (LGN_Id);
         Succs : constant LGE_List.Instance := Edges_To_Successors (LGN_Id);
         U_Id  : constant Unit_Id := Unit (LGN_Id);

      begin
         Write_Str  ("library graph node (LGN_Id_");
         Write_Int  (Int (LGN_Id));
         Write_Str  (") name = ");
         Write_Name (Name (LGN_Id));
         Write_Eol;

         if Present (Item) then
            Write_Str  ("  Corresponding_Item (LGN_Id_");
            Write_Int  (Int (Item));
            Write_Str  (") name = ");
            Write_Name (Name (Item));
         else
            Write_Str ("  Corresponding_Item (LGN_Id_0)");
         end if;

         Write_Eol;
         Write_Str ("  In_Elaboration_Order = ");

         if In_Elaboration_Order (LGN_Id) then
            Write_Str ("True");
         else
            Write_Str ("False");
         end if;

         Write_Eol;
         Write_Str ("  Pending_Predecessors = ");
         Write_Int (Int (Pending_Predecessors (LGN_Id)));
         Write_Eol;

         Write_Str ("  SCC (SCC_Id_");
         Write_Int (Int (SCC (LGN_Id)));
         Write_Str (")");
         Write_Eol;

         pragma Assert (Present (U_Id));

         Write_Str  ("  Unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (Name (U_Id));
         Write_Eol;

         if LGE_List.Is_Empty (Succs) then
            Write_Eol;
         else
            Write_Library_Graph_Edges (Succs);
         end if;
      end Write_Library_Graph_Node;

      -------------------------------
      -- Write_Library_Graph_Nodes --
      -------------------------------

      procedure Write_Library_Graph_Nodes is
         Iter   : LGN_List.Iterator;
         LGN_Id : Library_Graph_Node_Id;

      begin
         Iter := Iterate_Library_Graph_Nodes;
         while LGN_List.Has_Next (Iter) loop
            LGN_List.Next (Iter, LGN_Id);

            Write_Library_Graph_Node (LGN_Id);
         end loop;
      end Write_Library_Graph_Nodes;
   end Library_Graph_Writer;

   ----------
   -- Name --
   ----------

   function Name (U_Id : Unit_Id) return Unit_Name_Type is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames Units.Table (U_Id);

   begin
      return U_Rec.Uname;
   end Name;

   -------------
   -- Present --
   -------------

   function Present (Nam : Name_Id) return Boolean is
   begin
      return Nam /= No_Name;
   end Present;

   -------------------------
   -- Requires_Processing --
   -------------------------

   function Requires_Processing (U_Id : Unit_Id) return Boolean is
      pragma Assert (Present (U_Id));

      U_Rec : Unit_Record renames Units.Table (U_Id);

   begin
      --  Nothing to do for an interface to a stand-alone library

      if U_Rec.SAL_Interface then
         return False;
      end if;

      return True;
   end Requires_Processing;

   ---------------
   -- SCC_Graph --
   ---------------

   package body SCC_Graph is

      --------------------
      -- SCC graph node --
      --------------------

      --  The following type represents a SCC graph node

      type SCC_Graph_Node_Record is record
         Lib_Nodes : LGN_List.Instance := LGN_List.Nil;
         --  List of all library graph nodes which comprise this node

         Pending_Predecessors : Natural := 0;
         --  Number of pending predecessor SCC graph nodes that must be chosen
         --  in the elaboration order prior to this node.
      end record;

      ---------------------
      -- Data structures --
      ---------------------

      --  The following table stores the attributes of all SCC graph nodes

      package SCC_Attributes is new Table.Table
        (Table_Index_Type     => SCC_Graph_Node_Id,
         Table_Component_Type => SCC_Graph_Node_Record,
         Table_Low_Bound      => First_SCC_Graph_Node,
         Table_Initial        => 50,
         Table_Increment      => 200,
         Table_Name           => "SCC_Attributes");

      Iterable_SCCs : SCCGN_List.Instance := SCCGN_List.Nil;
      --  The iterable form of all SCCs

      ---------------------
      -- Build_SCC_Graph --
      ---------------------

      procedure Build_SCC_Graph is

         -----------
         -- Types --
         -----------

         --  The following type contains all attributes necessary for Tarjan's
         --  SCC algorithm. Index, Low_Link, and On_Stack are kept identical to
         --  those from the algorithm.
         --
         --  Stack_Pos was added for traceability and SCC graph node creation
         --  purposes.

         type Attribute_Record is record
            Index : Natural := 0;
            --  Visitation number

            Low_Link : Natural := Natural'Last;
            --  Lowest visitation number

            On_Stack : Boolean := False;
            --  Set when the library item appears in Stack

            Stack_Pos : Natural := 0;
            --  Position of the library item in Stack
         end record;

         ---------------------
         -- Data structures --
         ---------------------

         Global_Index : Natural := 0;
         --  The visitation number during Tarjan's SCC algorithm

         --  The following table stores the attributes of library graph nodes.
         --  This is a direct one-to-one mapping.

         package Attributes is new Table.Table
           (Table_Index_Type     => Library_Graph_Node_Id,
            Table_Component_Type => Attribute_Record,
            Table_Low_Bound      => First_Library_Graph_Node,
            Table_Initial        => Pos (Total_Library_Graph_Nodes),
            Table_Increment      => 200,
            Table_Name           => "Attributes");

         --  The following table represents the stack used in Tarjan's SCC
         --  algorithm.

         package Stack is new Table.Table
           (Table_Index_Type     => Natural,
            Table_Component_Type => Library_Graph_Node_Id,
            Table_Low_Bound      => 1,
            Table_Initial        => 50,
            Table_Increment      => 200,
            Table_Name           => "Stack");

         Visited_Set : LGN_Set.Instance := LGN_Set.Nil;
         --  The set of all visited library graph nodes during Tarjan's SCC
         --  algorithm.

         -----------------------
         -- Local subprograms --
         -----------------------

         procedure Build_SCC (LGN_Id : Library_Graph_Node_Id);
         pragma Inline (Build_SCC);
         --  Create a new SCC with a root library graph node denoted by id
         --  LGN_Id.

         function Index (LGN_Id : Library_Graph_Node_Id) return Natural;
         pragma Inline (Index);
         --  Obtain attribute Index of library graph node with id LGN_Id

         procedure Initialize_And_Push (LGN_Id : Library_Graph_Node_Id);
         pragma Inline (Initialize_And_Push);
         --  Initialize the attributes of library graph node with id LGN_Id.
         --  Push the node on Stack.

         procedure Insert_And_Pop
           (LGN_Id   : Library_Graph_Node_Id;
            SCCGN_Id : SCC_Graph_Node_Id);
         pragma Inline (Insert_And_Pop);
         --  Add library graph node with id LGN_Id to a SCC graph node with id
         --  SCCGN_Id.

         function Is_Visited (LGN_Id : Library_Graph_Node_Id) return Boolean;
         pragma Inline (Is_Visited);
         --  Determine whether library graph node with id LGN_Id has already
         --  been visited during a traversal.

         function Low_Link (LGN_Id : Library_Graph_Node_Id) return Natural;
         pragma Inline (Low_Link);
         --  Obtain attribute Low_Link of library graph node with id LGN_Id

         function On_Stack (LGN_Id : Library_Graph_Node_Id) return Boolean;
         pragma Inline (On_Stack);
         --  Obtain attribute On_Stack of library graph node with id LGN_Id

         procedure Set_Is_Visited
           (LGN_Id : Library_Graph_Node_Id;
            Val    : Boolean := True);
         pragma Inline (Set_Is_Visited);
         --  Mark library graph node with id LGN_Id as visited depending on Val

         procedure Set_Low_Link
           (LGN_Id : Library_Graph_Node_Id;
            Val    : Natural);
         pragma Inline (Set_Low_Link);
         --  Set attribute Low_Link of library graph node with id LGN_Id to Val

         procedure Set_On_Stack
           (LGN_Id : Library_Graph_Node_Id;
            Val    : Boolean := True);
         pragma Inline (Set_On_Stack);
         --  Set attribute On_Stack of library graph node with id LGN_Id to Val

         function Stack_Pos (LGN_Id : Library_Graph_Node_Id) return Natural;
         pragma Inline (Stack_Pos);
         --  Obtain attribute Stack_Pos of library graph node with id LGN_Id

         procedure Visit_Edge (LGE_Id : Library_Graph_Edge_Id);
         pragma Inline (Visit_Edge);
         --  Examine library graph edge with id LGE_Id and either
         --
         --    * Remove it if denotes a Body_Before_Spec edge
         --
         --    * Update the Pending_Predecessors of the successor's SCC

         procedure Visit_Edges;
         pragma Inline (Visit_Edges);
         --  Visit all library graph edges in order to
         --
         --    * Remove all Body_Before_Spec edges
         --
         --    * Update the Pending_Predecessors of all SCCs

         procedure Visit_Node (LGN_Id : Library_Graph_Node_Id);
         pragma Inline (Visit_Node);
         --  Visit library graph node with id LGN_Id in a DFS fashion

         procedure Visit_Nodes;
         pragma Inline (Visit_Nodes);
         --  Start a DFS traversal from each library graph node

         procedure Visit_Successors (LGN_Id : Library_Graph_Node_Id);
         pragma Inline (Visit_Successors);
         --  Visit the successors of a library graph node with id LGN_Id in a
         --  DFS fashion.

         ---------------
         -- Build_SCC --
         ---------------

         procedure Build_SCC (LGN_Id : Library_Graph_Node_Id) is
            From : constant Natural := Stack_Pos (LGN_Id);
            To   : constant Natural := Stack.Last;
            --  The range of library graph nodes on the stack which comprise
            --  the SCC. Since the input node is the SCC root, it provides the
            --  low bound of the range.

            SCCGN_Rec : constant SCC_Graph_Node_Record :=
                          (Lib_Nodes            => LGN_List.Create,
                           Pending_Predecessors => 0);
            --  The number of pending predecessors is left as 0, but is later
            --  set in Visit_Edges.

            SCCGN_Id : SCC_Graph_Node_Id;

         begin
            --  Create the attributes of the SCC

            SCC_Attributes.Append (SCCGN_Rec);
            SCCGN_Id := SCC_Attributes.Last;

            --  Add the SCC to the list of iterable SCCs

            SCCGN_List.Append (Iterable_SCCs, SCCGN_Id);

            --  The SCC must consist of at least the root library graph node

            pragma Assert (From <= To);

            --  Insert each library graph node in the SCC and mark it as no
            --  longer being on Stack.

            for Index in From .. To loop
               Insert_And_Pop
                 (LGN_Id   => Stack.Table (Index),
                  SCCGN_Id => SCCGN_Id);
            end loop;

            --  Pop all library graph nodes which comprise the SCC

            Stack.Set_Last (From - 1);
         end Build_SCC;

         -----------
         -- Index --
         -----------

         function Index (LGN_Id : Library_Graph_Node_Id) return Natural is
         begin
            pragma Assert (Present (LGN_Id));
            return Attributes.Table (LGN_Id).Index;
         end Index;

         -------------------------
         -- Initialize_And_Push --
         -------------------------

         procedure Initialize_And_Push (LGN_Id : Library_Graph_Node_Id) is
         begin
            pragma Assert (Present (LGN_Id));

            Attributes.Table (LGN_Id) :=
              (Index      => Global_Index,
               Low_Link   => Global_Index,
               On_Stack   => True,
               Stack_Pos  => Stack.Last + 1);

            --  Push the library graph node on Stack. Note that the stack
            --  position recorded above takes into account the push.

            Stack.Append (LGN_Id);

            --  Prepare the visitation number for the next node

            Global_Index := Global_Index + 1;
         end Initialize_And_Push;

         --------------------
         -- Insert_And_Pop --
         --------------------

         procedure Insert_And_Pop
           (LGN_Id   : Library_Graph_Node_Id;
            SCCGN_Id : SCC_Graph_Node_Id)
         is
         begin
            pragma Assert (Present (LGN_Id));
            pragma Assert (Present (SCCGN_Id));

            --  The library graph node must still be on Stack

            pragma Assert (On_Stack (LGN_Id));

            --  The node must not be associated with a SCC

            pragma Assert (not Present (SCC (LGN_Id)));

            --  Associate the node with the SCC

            LGN_List.Append (Lib_Nodes (SCCGN_Id), LGN_Id);
            Set_SCC (LGN_Id, SCCGN_Id);

            --  Mark the node as not being on Stack. The node is popped in
            --  routine Build_SCC.

            Set_On_Stack (LGN_Id, False);
         end Insert_And_Pop;

         ----------------
         -- Is_Visited --
         ----------------

         function Is_Visited (LGN_Id : Library_Graph_Node_Id) return Boolean is
         begin
            pragma Assert (Present (LGN_Id));
            return LGN_Set.Contains (Visited_Set, LGN_Id);
         end Is_Visited;

         --------------
         -- Low_Link --
         --------------

         function Low_Link (LGN_Id : Library_Graph_Node_Id) return Natural is
         begin
            pragma Assert (Present (LGN_Id));
            return Attributes.Table (LGN_Id).Low_Link;
         end Low_Link;

         --------------
         -- On_Stack --
         --------------

         function On_Stack (LGN_Id : Library_Graph_Node_Id) return Boolean is
         begin
            pragma Assert (Present (LGN_Id));
            return Attributes.Table (LGN_Id).On_Stack;
         end On_Stack;

         --------------------
         -- Set_Is_Visited --
         --------------------

         procedure Set_Is_Visited
           (LGN_Id : Library_Graph_Node_Id;
            Val    : Boolean := True)
         is
         begin
            if Val then
               LGN_Set.Insert (Visited_Set, LGN_Id);
            else
               LGN_Set.Delete (Visited_Set, LGN_Id);
            end if;
         end Set_Is_Visited;

         ------------------
         -- Set_Low_Link --
         ------------------

         procedure Set_Low_Link
           (LGN_Id : Library_Graph_Node_Id;
            Val    : Natural)
         is
         begin
            pragma Assert (Present (LGN_Id));
            Attributes.Table (LGN_Id).Low_Link := Val;
         end Set_Low_Link;

         ------------------
         -- Set_On_Stack --
         ------------------

         procedure Set_On_Stack
           (LGN_Id : Library_Graph_Node_Id;
            Val    : Boolean := True)
         is
         begin
            pragma Assert (Present (LGN_Id));
            Attributes.Table (LGN_Id).On_Stack := Val;
         end Set_On_Stack;

         ---------------
         -- Stack_Pos --
         ---------------

         function Stack_Pos (LGN_Id : Library_Graph_Node_Id) return Natural is
         begin
            pragma Assert (Present (LGN_Id));
            return Attributes.Table (LGN_Id).Stack_Pos;
         end Stack_Pos;

         ----------------
         -- Visit_Edge --
         ----------------

         procedure Visit_Edge (LGE_Id : Library_Graph_Edge_Id) is
            pragma Assert (Present (LGE_Id));

            Pred : constant Library_Graph_Node_Id := Predecessor (LGE_Id);
            Succ : constant Library_Graph_Node_Id := Successor   (LGE_Id);

            Pred_SCC : SCC_Graph_Node_Id;
            Succ_SCC : SCC_Graph_Node_Id;

         begin
            --  The library graph edge denotes a special Body_Before_Spec edge
            --  that links a successor spec with a predecessor body. Eliminate
            --  the edge to break the intentional circularity between the spec
            --  and body.

            if Kind (LGE_Id) = Body_Before_Spec_Edge then
               Remove_Body_Before_Spec_Edge
                 (LGE_Id => LGE_Id,
                  Pred   => Pred,
                  Succ   => Succ);

            --  Otherwise the edge is a normal edge

            else
               Pred_SCC := SCC (Pred);
               Succ_SCC := SCC (Succ);

               pragma Assert (Present (Pred_SCC));
               pragma Assert (Present (Succ_SCC));

               --  The edge links a successor and a predecessor coming from two
               --  different SCCs. This indicates that the SCC of the successor
               --  must wait for another predecessor until it can be chosen for
               --  the elaboration order.

               if Pred_SCC /= Succ_SCC then
                  Set_Pending_Predecessors
                    (Succ_SCC, Pending_Predecessors (Succ_SCC) + 1);
               end if;
            end if;
         end Visit_Edge;

         -----------------
         -- Visit_Edges --
         -----------------

         procedure Visit_Edges is
            Iter   : LGE_List.Iterator;
            LGE_Id : Library_Graph_Edge_Id;

         begin
            Iter := Iterate_Library_Graph_Edges;
            while LGE_List.Has_Next (Iter) loop
               LGE_List.Next (Iter, LGE_Id);

               Visit_Edge (LGE_Id);
            end loop;

            --  Eliminate all Body_Before_Spec library graph edges from
            --  internal data structures.

            Remove_Body_Before_Spec_Edges;
         end Visit_Edges;

         ----------------
         -- Visit_Node --
         ----------------

         procedure Visit_Node (LGN_Id : Library_Graph_Node_Id) is
         begin
            pragma Assert (Present (LGN_Id));

            --  Nothing to do when the library graph node has already been
            --  visited.

            if Is_Visited (LGN_Id) then
               return;
            end if;

            --  Mark the node as visited

            Set_Is_Visited (LGN_Id);

            --  Initialize the state of the node, and push it on Stack

            Initialize_And_Push (LGN_Id);

            --  A library graph node requires a special Body_Before_Spec edge
            --  to its Corresponging_Item when it either denotes a
            --
            --    * Body that completes a previous spec
            --
            --    * Spec with a completing body
            --
            --  The edge creates an intentional circularity between the spec
            --  and body in order to emulate a library unit, and guarantees
            --  that both will appear in the same SCC. The edge is removed in
            --  routine Visit_Edge.
            --
            --  Due to the structure of the library graph, either the spec or
            --  the body may be visited first, yet Corresponding_Item will
            --  still attempt to create the Body_Before_Spec edge. This is OK
            --  because successor and predecessor are kept consistent in both
            --  cases, and Build_Library_Graph_Edge will prevent the creation
            --  of the second edge.

            --  A body that completes a previous spec

            if Is_Body_With_Spec (LGN_Id) then
               Build_Library_Graph_Edge
                 (Pred => LGN_Id,                        --  body
                  Succ => Corresponding_Item (LGN_Id),   --  spec
                  Kind => Body_Before_Spec_Edge);

            --  A spec with a completing body

            elsif Is_Spec_With_Body (LGN_Id) then
               Build_Library_Graph_Edge
                 (Pred => Corresponding_Item (LGN_Id),   --  body
                  Succ => LGN_Id,                        --  spec
                  Kind => Body_Before_Spec_Edge);
            end if;

            Visit_Successors (LGN_Id);

            --  The node is the root node of a SCC, create a new SCC

            if Low_Link (LGN_Id) = Index (LGN_Id) then
               Build_SCC (LGN_Id);
            end if;
         end Visit_Node;

         -----------------
         -- Visit_Nodes --
         -----------------

         procedure Visit_Nodes is
            Iter   : LGN_List.Iterator;
            LGN_Id : Library_Graph_Node_Id;

         begin
            Iter := Iterate_Library_Graph_Nodes;
            while LGN_List.Has_Next (Iter) loop
               LGN_List.Next (Iter, LGN_Id);

               Visit_Node (LGN_Id);
            end loop;
         end Visit_Nodes;

         ----------------------
         -- Visit_Successors --
         ----------------------

         procedure Visit_Successors (LGN_Id : Library_Graph_Node_Id) is
            Iter   : LGE_List.Iterator;
            LGE_Id : Library_Graph_Edge_Id;
            Succ   : Library_Graph_Node_Id;

         begin
            Iter := LGE_List.Iterate (Edges_To_Successors (LGN_Id));
            while LGE_List.Has_Next (Iter) loop
               LGE_List.Next (Iter, LGE_Id);

               pragma Assert (Predecessor (LGE_Id) = LGN_Id);
               Succ := Successor (LGE_Id);

               --  The current successor has not been visited yet

               if not Is_Visited (Succ) then
                  Visit_Node (Succ);

                  Set_Low_Link
                    (LGN_Id, Natural'Min (Low_Link (LGN_Id), Low_Link (Succ)));

               --  The current successor has been visited, and still remains on
               --  the stack which indicates that it does not participate in a
               --  SCC yet.

               elsif On_Stack (Succ) then
                  Set_Low_Link
                    (LGN_Id, Natural'Min (Low_Link (LGN_Id), Index (Succ)));
               end if;
            end loop;
         end Visit_Successors;

      --  Start of processing for Build_SCC_Graph

      begin
         Visited_Set := LGN_Set.Create (Total_Library_Graph_Nodes);

         --  Run Tarjan's SCC algorithm to discover all SCCs in the library
         --  graph.

         Visit_Nodes;

         --  Remove all special Body_Before_Spec library edges and update the
         --  Pending_Predecessors of all SCCs.

         Visit_Edges;

         LGN_Set.Destroy (Visited_Set);
         Validate_SCC_Graph;
         Write_SCC_Graph;
      end Build_SCC_Graph;

      ------------------------
      -- Finalize_SCC_Graph --
      ------------------------

      procedure Finalize_SCC_Graph is
      begin
         SCCGN_List.Destroy (Iterable_SCCs);
      end Finalize_SCC_Graph;

      --------------------------
      -- Initialize_SCC_Graph --
      --------------------------

      procedure Initialize_SCC_Graph is
      begin
         Iterable_SCCs := SCCGN_List.Create;
      end Initialize_SCC_Graph;

      -----------------------------
      -- Iterate_SCC_Graph_Nodes --
      -----------------------------

      function Iterate_SCC_Graph_Nodes return SCCGN_List.Iterator is
      begin
         return SCCGN_List.Iterate (Iterable_SCCs);
      end Iterate_SCC_Graph_Nodes;

      ---------------
      -- Lib_Nodes --
      ---------------

      function Lib_Nodes
        (SCCGN_Id : SCC_Graph_Node_Id) return LGN_List.Instance
      is
      begin
         pragma Assert (Present (SCCGN_Id));
         return SCC_Attributes.Table (SCCGN_Id).Lib_Nodes;
      end Lib_Nodes;

      --------------------------
      -- Pending_Predecessors --
      --------------------------

      function Pending_Predecessors
        (SCCGN_Id : SCC_Graph_Node_Id) return Natural
      is
      begin
         pragma Assert (Present (SCCGN_Id));
         return SCC_Attributes.Table (SCCGN_Id).Pending_Predecessors;
      end Pending_Predecessors;

      -------------
      -- Present --
      -------------

      function Present (SCCGN_Id : SCC_Graph_Node_Id) return Boolean is
      begin
         return SCCGN_Id /= No_SCC_Graph_Node;
      end Present;

      ------------------------------
      -- Set_Pending_Predecessors --
      ------------------------------

      procedure Set_Pending_Predecessors
        (SCCGN_Id : SCC_Graph_Node_Id;
         Val      : Natural)
      is
      begin
         pragma Assert (Present (SCCGN_Id));
         SCC_Attributes.Table (SCCGN_Id).Pending_Predecessors := Val;
      end Set_Pending_Predecessors;

      ----------------
      -- Total_SCCs --
      ----------------

      function Total_SCCs return Natural is
      begin
         return SCCGN_List.Size (Iterable_SCCs);
      end Total_SCCs;
   end SCC_Graph;

   -------------------------
   -- SCC_Graph_Validator --
   -------------------------

   package body SCC_Graph_Validator is

      Has_Invalid_Data : Boolean := False;
      --  Flag set when the SCC graph contains invalid data

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Validate_SCC (SCCGN_Id : SCC_Graph_Node_Id);
      pragma Inline (Validate_SCC);
      --  Verify a SCC with id SCCGN_Id

      procedure Validate_SCC_Graph_Node (LGN_Id : Library_Graph_Node_Id);
      pragma Inline (Validate_SCC_Graph_Node);
      --  Verify a library graph node with id LGN_Id

      procedure Validate_SCC_Graph_Nodes;
      pragma Inline (Validate_SCC_Graph_Nodes);
      --  Verify all library graph nodes

      procedure Validate_SCCs;
      pragma Inline (Validate_SCCs);
      --  Verify all SCCs

      procedure Write_Error (Msg : String);
      pragma Inline (Write_Error);
      --  ???

      ------------------
      -- Validate_SCC --
      ------------------

      procedure Validate_SCC (SCCGN_Id : SCC_Graph_Node_Id) is
         Msg : constant String := "Validate_SCC";

      begin
         if not Present (SCCGN_Id) then
            Write_Error (Msg);

            Write_Str ("  empty SCC");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if LGN_List.Is_Empty (Lib_Nodes (SCCGN_Id)) then
            Write_Error (Msg);

            Write_Str ("  SCC (SCCGN_Id_");
            Write_Int (Int (SCCGN_Id));
            Write_Str (") must contain at least one library graph node");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_SCC;

      ------------------------
      -- Validate_SCC_Graph --
      ------------------------

      procedure Validate_SCC_Graph is
      begin
         --  Nothing to do when switch -d_V (validate bindo graphs and order)
         --  is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Validate_SCC_Graph_Nodes;
         Validate_SCCs;

         if Has_Invalid_Data then
            raise Invalid_SCC_Graph;
         end if;
      end Validate_SCC_Graph;

      -----------------------------
      -- Validate_SCC_Graph_Node --
      -----------------------------

      procedure Validate_SCC_Graph_Node (LGN_Id : Library_Graph_Node_Id) is
         Msg : constant String := "Validate_SCC_Graph_Node";

      begin
         if not Present (LGN_Id) then
            Write_Error (Msg);

            Write_Str ("  empty library graph node");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if not Present (SCC (LGN_Id)) then
            Write_Error (Msg);

            Write_Str ("  library graph node (LGN_Id_");
            Write_Int (Int (LGN_Id));
            Write_Str (") lacks SCC");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_SCC_Graph_Node;

      ------------------------------
      -- Validate_SCC_Graph_Nodes --
      ------------------------------

      procedure Validate_SCC_Graph_Nodes is
         Iter   : LGN_List.Iterator;
         LGN_Id : Library_Graph_Node_Id;

      begin
         Iter := Iterate_Library_Graph_Nodes;
         while LGN_List.Has_Next (Iter) loop
            LGN_List.Next (Iter, LGN_Id);

            Validate_SCC_Graph_Node (LGN_Id);
         end loop;
      end Validate_SCC_Graph_Nodes;

      -------------------
      -- Validate_SCCs --
      -------------------

      procedure Validate_SCCs is
         Iter     : SCCGN_List.Iterator;
         SCCGN_Id : SCC_Graph_Node_Id;

      begin
         Iter := Iterate_SCC_Graph_Nodes;
         while SCCGN_List.Has_Next (Iter) loop
            SCCGN_List.Next (Iter, SCCGN_Id);

            Validate_SCC (SCCGN_Id);
         end loop;
      end Validate_SCCs;

      -----------------
      -- Write_Error --
      -----------------

      procedure Write_Error (Msg : String) is
      begin
         Has_Invalid_Data := True;

         Write_Str ("ERROR: ");
         Write_Str (Msg);
         Write_Eol;
      end Write_Error;
   end SCC_Graph_Validator;

   ----------------------
   -- SCC_Graph_Writer --
   ----------------------

   package body SCC_Graph_Writer is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_SCC (SCCGN_Id : SCC_Graph_Node_Id);
      pragma Inline (Write_SCC);
      --  Write out the data for a SCC with id SCCGN_Id

      procedure Write_SCC_Graph_Node (LGN_Id : Library_Graph_Node_Id);
      pragma Inline (Write_SCC_Graph_Node);
      --  Write out the data of library graph node with id LGN_Id to standard
      --  output.

      procedure Write_SCC_Graph_Nodes (Nodes : LGN_List.Instance);
      pragma Inline (Write_SCC_Graph_Nodes);
      --  Write out the data of library graph nodes Nodes to standard output

      procedure Write_SCCs;
      pragma Inline (Write_SCCs);
      --  Write out the data of all SCC to standard output

      -----------
      -- Debug --
      -----------

      procedure pscc (SCCGN_Id : SCC_Graph_Node_Id)
        renames Write_SCC;
      pragma Unreferenced (pscc);

      procedure psccgn (LGN_Id : Library_Graph_Node_Id)
        renames Write_SCC_Graph_Node;
      pragma Unreferenced (psccgn);

      ---------------
      -- Write_SCC --
      ---------------

      procedure Write_SCC (SCCGN_Id : SCC_Graph_Node_Id) is
         pragma Assert (Present (SCCGN_Id));

         Nodes : constant LGN_List.Instance := Lib_Nodes (SCCGN_Id);

      begin
         Write_Str ("SCC (SCCGN_Id_");
         Write_Int (Int (SCCGN_Id));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Pending_Predecessors = ");
         Write_Int (Int (Pending_Predecessors (SCCGN_Id)));
         Write_Eol;

         if LGN_List.Is_Empty (Nodes) then
            Write_Eol;
         else
            Write_SCC_Graph_Nodes (Nodes);
         end if;
      end Write_SCC;

      ---------------------
      -- Write_SCC_Graph --
      ---------------------

      procedure Write_SCC_Graph is
      begin
         --  Nothing to do when switch -d_S (output SCC graph) is not in effect

         if not Debug_Flag_Underscore_SS then
            return;
         end if;

         Write_Str ("SCC Graph");
         Write_Eol;
         Write_Eol;

         Write_Str ("SCCs: ");
         Write_Num (Int (Total_SCCs));
         Write_Eol;
         Write_Eol;

         Write_SCCs;

         Write_Str ("SCC Graph end");
         Write_Eol;

         Write_Eol;
      end Write_SCC_Graph;

      --------------------------
      -- Write_SCC_Graph_Node --
      --------------------------

      procedure Write_SCC_Graph_Node (LGN_Id : Library_Graph_Node_Id) is
      begin
         pragma Assert (Present (LGN_Id));

         Write_Str  ("    library graph node (LGN_Id_");
         Write_Int  (Int (LGN_Id));
         Write_Str  (") name = ");
         Write_Name (Name (LGN_Id));
         Write_Eol;
      end Write_SCC_Graph_Node;

      ---------------------------
      -- Write_SCC_Graph_Nodes --
      ---------------------------

      procedure Write_SCC_Graph_Nodes (Nodes : LGN_List.Instance) is
         Iter   : LGN_List.Iterator;
         LGN_Id : Library_Graph_Node_Id;

      begin
         Write_Str ("  Lib_Nodes");
         Write_Eol;

         Iter := LGN_List.Iterate (Nodes);
         while LGN_List.Has_Next (Iter) loop
            LGN_List.Next (Iter, LGN_Id);

            Write_SCC_Graph_Node (LGN_Id);
         end loop;

         Write_Eol;
      end Write_SCC_Graph_Nodes;

      ----------------
      -- Write_SCCs --
      ----------------

      procedure Write_SCCs is
         Iter     : SCCGN_List.Iterator;
         SCCGN_Id : SCC_Graph_Node_Id;

      begin
         Iter := Iterate_SCC_Graph_Nodes;
         while SCCGN_List.Has_Next (Iter) loop
            SCCGN_List.Next (Iter, SCCGN_Id);

            Write_SCC (SCCGN_Id);
         end loop;
      end Write_SCCs;
   end SCC_Graph_Writer;

   -----------------
   -- Total_Units --
   -----------------

   function Total_Units return Natural is
   begin
      return Natural (Units.Last) - Natural (Units.First) + 1;
   end Total_Units;

   ---------------
   -- Write_Num --
   ---------------

   procedure Write_Num (Val : Int) is
      function Number_Of_Digits return Int;
      pragma Inline (Number_Of_Digits);
      --  ???

      ----------------------
      -- Number_Of_Digits --
      ----------------------

      function Number_Of_Digits return Int is
         Count : Int;
         Num   : Int;

      begin
         --  Treat zero as a single digit

         if Val = 0 then
            return 1;
         end if;

         Count := 0;
         Num   := Val;

         --  Shrink the input value by dividing it until all of its digits are
         --  exhausted.

         while Num /= 0 loop
            Count := Count + 1;
            Num   := Num / 10;
         end loop;

         return Count;
      end Number_Of_Digits;

   --  Start of processing for Write_Num

   begin
      for Space in 1 .. Number_Alignment_Column - Number_Of_Digits loop
         Write_Char (' ');
      end loop;

      Write_Int (Val);
   end Write_Num;

end Bindo;
