---
title: CSS Flexbox Grid
tags: prog, cloud, frontend, js
---

# Flex vs Grid

* the flex children's `flex:` attribute is very similar to Grid's `fr` fractional positioning.  
    * childA has `flex: 1` , childB has `flex: 2` means childA takes up 33.3% space and childB takes up 66.6%
    * `grid-template-columns: 1fr 2fr` means 2 grid columns with width 33.3% and 66.6%

Might be more difficult with flex objects since you have to keep track of the total sum of all the flex children's `flex` attribute

# Flex

## Flex

* main css properties
  * `display:flex;`
  * `flex-direction: row;`
  * `justify-content: space-between;`
  * `align-items: center`

use `display: flex` on parent of list of items.

```html
<div id="BunchAThings" style='display:flex'>
    <div class="someFlexChildA">
    <div class="someFlexChildB">
    <div class="someFlexChildC">
</div>
```

```html
#BunchAThings{
    display: flex;

    flex-direction: row;

    justify-content: space-between;

    align-items: center
}

.someFlexChildB{
    order: -1;
    align-self: center;
}
```

* if flex-direction is row 
  * then `justify-content` determines alignment of objects in the row. 
  * then `align-items` determines alignment of objects in the orthogonal direction AKA column.


## Operating on flex children

* flex children with class `someFlexChildB`
  * `order:-1` means someFlexChildB will be first (assuming all other flex children have order 0 as default)
  * `align-self:center` basically same as `align-items` but specifically for this class of children



## Wrap and align-content

* `wrap` does wrapping
  * `align-content` determines spacing between wrapped content  


`flex-flow` is just a shorthand that literally combines `flex-direction` `wrap`



```html
.someFlexItemB {
    flex: 1 1 0;
}

.constantFlexItemSize{
    flex-grow: 0;
    flex-shrink: 0;
    flex-basis: 100px;
}
```
* flex: 1; is equivalent to flex: 1 1 0;
  * flex-grow : 1;    ➜ how much it grows: higher number cause more growth, 0 for no growth    
  * flex-shrink : 1;  ➜ how much it shrinks: higher number cause more shrinkage, 0 for no shrink
  * flex-basis : 0;   ➜ Starting size on load


https://almanac.httparchive.org/en/2021/css#flexbox-and-grid-adoption




# Chrome Inspect

Go to Chrome Inspect > Layout  
This will show all Grids and Flexbox used on a website. Notice most sites use flexbox.


# Grid 

https://grid.layoutit.com/

* grid lines start at index 1

```html
.mygrid{

    display: grid;
    <!-- below are 2 ways to make 5 equidistant cells resulting in 5x5 grid -->
    grid-template-columns: repeat(5,20%);
    grid-template-rows: 20% 20% 20% 20% 20%;

    #childcellA {
        grid-column: 2/5
        <!-- start from grid line 2 and end at grid line 5 -->
    }

    #childcellB {
        grid-column: 2/3 span
        <!-- start from grid line 2 and length of 3 forward to line 5 -->
    }

    #childcellB {
        grid-column: span 3/5
        <!-- length of 3 from grid line 5 backwards to line 2 -->
    }

    #childcellC{
        grid-column-start: span 3;
        grid-column-end: 5
        <!-- length of 3 from grid line 5 backwards to line 2-->
    }

    #childcellD {
        grid-column-start: 2;
        grid-column-end: span 3;
        <!-- length of 3 from grid line 2 forward  to line 5-->
    }

}
```

All the child cells cover the same 3 cells from line 2 to line 5.

## grid-area

grid-area starts at (y=1,x=1) which is upper left corner.  
Remember grid is indexed 1.

* `grid-area: grid-row-start / grid-column-start / grid-row-end / grid-column-end`
  * `grid-area: 1 / 2 / 4 / 6`
  * start at (y=1, x= 2)
  * end at (y=4,x=6)

```html

    #childcellThreeByFour {
        grid-area: 1/2/4/6;
        <!-- starting on (y=1,x=2) cover area by 3x4 ending at (y=4,x=6)-->
    }

<!-- the grid is shown below 
x o o o o
x o o o o
x o o o o
x x x x x
x x x x x 
-->
```

## grid building

The number of columns is the number of parameters given to grid-template-column.  
Since we have 3 elements (1fr, 5fr, 2fr) then we have 3 columns.

fractionals `fr` will auto divide by the total number of `fr` fractional parameters to get percent of grid.  
Clearly this means `2fr 2fr` == `1fr 1fr` since `2/4 2/4` == `1/2 1/2`  

 * `grid-template-columns: 1fr 5fr 2fr;`
   * makes 3 columns spaced by 1/(1+5+2) 5/(1+5+2) 2/(1+5+2)
   * aka 3 columns of width 12.5% 62.5% 25%

Fractional auto-fills the rest of the grid

* `grid-template-columns: 25px 1fr 1fr;`
  * means 3 columns, 1st is 25 pixels wide, 2nd is 50% aka 1/2, 3rd is 50% aka 1/2
  * here rest of the grid which excludes the first 25 pixel represents 100% fractional.

### grid-template shorthand

* `grid-template: row-width-parameters / column-width-parameters 
* `grid-template: 1fr 2fr / 1fr 1fr;`
  * makes 2x2 rows x columns





# Minimal Layout examples

## flexbox

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Minimal layout using flexbox</title>

  </head>
  <body>
    <section>
      <style>
        .body-container { 
          display: flex;
          flex-direction: column;
          justify-content: space-around;
          }
          
         /*body-container contains {header,main,footer}*/
        .body-container > header {
         display: flex; 
         min-height: 10vh;
        }

        .body-container >  footer {
          background: green;
          padding: 1em; 
          min-height: 10vh;
        }
        .body-container > main {
          background: purple;
          display: flex; 
          flex-direction: row;
          min-height: 80vh;
        }
        
        /*main contains {red, green, blue} */
        main > .red {
          background: orangered;
        }
        main > .green {
          background: yellowgreen;
          flex: 2 !important;
        }
        main > .blue {
          background: steelblue;
        }
        .body-container > main > div {
          margin: 1vw;
          flex: 1 ;
        }

        
      </style>
      <div class="body-container">
        <div class="header">
          <div class="top-navbar"> 
            <a href="#">Logo Here</a>
          </div>
        </div>

        <main>
          Main
          <div class="red">1</div>
          <div class="green">2</div>
          <div class="blue">3</div>
        </main>
        <footer>Ending</footer>
      </div>    
    </section>
    
  </body>
</html>

```