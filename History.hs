-- The following exports everything. 
-- That could be done more easily by leaving out the export list!
module History (applyAChange, Change, History(..), redo, undo)
where 
-- A History object has two stacks. The type of object is unknown
-- and is referred to with the type parameter "a". 

-- The first stack stores the history. The top of the stack is the 
-- current object state. The bottom of the stack is the start of
-- this history.

-- The second stack is the redo stack. It stores objects (i.e.,
-- states) removed from the history stack by undo and makes them
-- available for redo.

-- We access the stacks only by pattern matching and don't give
-- them names.
data History a = History {- history stack -} [a] {- redo stack -} [a]

-- Following are the generic history operations. The type of objects
-- in the history and redo stacks is not known. I'll refer to them
-- simply as objects or object states. An individual object (state)
-- will be o; a list of them will be os.

instance (Show a) => Show (History a) where
  -- Convert the current History object to a string.

  -- The first clause requires that the history stack not be empty.
  -- Using "_" in front of a variable name means (by convention) that
  -- we don't intend to use it. Could have written (History (o:_) _)
  -- This clause expects a History argument. The call to show on the
  -- right expects an argument to which show applies. That's why the
  -- instance declaration requries that type "a" be of class Show. 
  show (History (o:_historyStackTail) _redoStack) = show o 
  -- The second clause applies only if the history stack is empty.
  show _ = "Nothing to show." 

-- A function of type (Change a) converts an object of type a to 
-- another object of type a.
type Change a = a -> a

-- Return a function that makes the indicated change to the current
-- object state and update History. Since redo makes sense only
-- after undo, discard the redo stack.
applyAChange :: Change a -> Change (History a)
applyAChange f = \(History (o : os) _) -> History (f o : o : os) []

-- Pop an object state from the redo stack and push it onto the
-- history stack, making it the current object state. If the redo
-- stack is empty do nothing.
redo :: Change (History a)
redo (History hStack (o:redoStack)) = History (o:hStack) redoStack 
redo history = history

-- Pop the current object state from the history stack (revealing
-- the previous object state) and push it onto the redo stack.
undo :: Change (History a)
-- Can't undo if the history stack is empty.
undo (History (o:os) redoStack) = History os (o:redoStack)
undo history = history
