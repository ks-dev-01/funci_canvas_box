# funci_canvas_box
Testing Avalonia FuncUi

**NOTE:** The issue in this repository has been fixed with a new release of FuncUi.
Troubleshooting learning with FuncUI

I ran into an issue when creating a nested view, in a nested view. 

The shell allows two views:
- A PageOther View which is a simple view with a text box, and a text block, in a vertical view
- A PageSelector which, on the left hand side has a ListBox with several items, that when clicked will show a different view on the right hand side
  - The List is of the follow page views / types
    - PageOther
    - PageSimple
      - Same as PageOther, but arranged horizontally instead of vertically
  - PageCanvas

Previously, the PageSelector code was the Shell code and worked, but when I moved the logic from the Shell, into Page Selector, (nesting a view in a view), it no longer worked.

On line 37 / 38 in Shell.fs, if I swap these out and restart, it will show the right page in the initial view. But when I click either button it does not work.


