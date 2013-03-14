# Table Editor

## Overview

*Table Editor* is a package for the *Sublime Text 2* editor for edit text tables. 
*Table Editor* is very similar to Emacs-org mode table editor with almost the same keys. 

*Table Editor* allow on easy way edit text table, it allows:

- navigate with tab/shift tab 
- insert/delete row
- insert/delete column
- auto align number cells to right, text cells to left, header cells to center
- move column left/right
- move row up/down
- split long cell
- join two rows into one
- convert selected CSV region into table
- support single hline with character '-'
- support double hline with character '='
- direct support subset of wiki table syntax
    - Simple
    - EmacsOrgMode
    - Pandoc
    - Multi Markdown
    - reStructuredText
    - Textile
- auto detect table syntax by view syntax
- switch between different table syntax on the fly
- temporary disable/enable table editor
- customize table syntax
- show demo film in scratch view

## Usage

### Basic editing

For first time you should enable table editor with command palette:

* click *ctrl+shift+p*
* select *Table Editor: Enable for current syntax* or *Table Editor: Enable for current view* or "Table Editor: Set table syntax ... for current view"

Then when *Table Editor* is enabled, type

    | Name | Phone |
    |-

Then press *Tab* key, you will get pretty printed table

    | Name | Phone |
    |------|-------|
    | _    |       |

Then fill a data and press *Tab* key to navigate to next field or add new row if necessary 

    |    Name   |   Phone   |
    |-----------|-----------|
    | Anna      | 123456789 |
    | Alexander | 987654321 |
    | _         |           |

For make table a bit faster faster type only

    |Name|Phone

And then click *ctrl+k,enter*. 

    | Name | Phone |
    |------|-------|
    | _    |       |

*Table Editor* support double hline with character '='. Type bellow 

    | Name | Phone |
    |=

and click *tab* key

    | Name | Phone |
    |======|=======|
    | _    |       |

Then fill rows and click *ctrl+k,enter* each time when cursor in *Phone* position

    |    Name   |   Phone   |
    |===========|===========|
    | Anna      | 123456789 |
    |-----------|-----------|
    | Alexander | 987654321 |
    |-----------|-----------|
    | _         |           |


Additional to *tab* and *shift+tab* use *enter*  for move cursor down and insert new row if necessary.

### Work with columns

Let's we have a table with columns *| Name | Phone |*, and you decide insert column *| Age |* before column *| Phone |*.
For do this set cursor position into any rows in column Phone

    |    Name   |   Phone   |
    |-----------|-----------|
    | Anna      | 123456789 |
    | Alexander | 987654321 |
    |           | _         |

Click *alt+shift+right*

    |    Name   |   |   Phone   |
    |-----------|---|-----------|
    | Anna      |   | 123456789 |
    | Alexander |   | 987654321 |
    |           | _ |           |

Fill *| Age |* column

    |    Name   | Age |   Phone   |
    |-----------|-----|-----------|
    | Anna      |  32 | 123456789 |
    | Alexander |  28_| 987654321 |
    |           |     |           |

Then after some thought you decide switch columns *| Age |* and *| Phone |*. For do this, you can click *alt+right* when 
cursor in the *| Age |* column or you can click *alt+left* when cursor position in the *| Phone |* column

    |    Name   |   Phone   | Age |
    |-----------|-----------|-----|
    | Anna      | 123456789 | 32  |
    | Alexander | 987654321 | 28_ |
    |           |           |     |

Now cursor position in the *| Age |* column, when you click *ctrl+shift+left*, column *| Age |* will be deleted

    |    Name   |   Phone    |
    |-----------|------------|
    | Anna      | 123456789  |
    | Alexander | 987654321_ |
    |           |            |


### Work with rows

Let's we have a table

    |    Name   |   Phone   | Age |
    |-----------|-----------|-----|
    | Anna      | 123456789 | 32_ |
    | Alexander | 987654321 | 28  |
    |           |           |     |

For insert row bellow current cursor position click *alt+shift+down*

    |    Name   |   Phone   | Age |
    |-----------|-----------|-----|
    |           |           | _   |
    | Anna      | 123456789 | 32  |
    | Alexander | 987654321 | 28  |
    |           |           |     |

For delete row click *alt_shift+up*

    |    Name   |   Phone   | Age |
    |-----------|-----------|-----|
    | Anna      | 123456789 | 32_ |
    | Alexander | 987654321 | 28  |
    |           |           |     |

Some time you cell value became to long as in next example column *| Position |*

    |    Name   |   Phone   | Age |             Position             |
    |-----------|-----------|-----|----------------------------------|
    | Anna      | 123456789 |  32 | Senior Software Engineer_        |
    | Alexander | 987654321 |  28 | Senior Software Testing Engineer |
    |           |           |     |                                  |

You like to split value of column *| Position |* into several rows.
First let's click *ctrl+k,-* for insert hline after cursor position

    |    Name   |   Phone   | Age |             Position             |
    |-----------|-----------|-----|----------------------------------|
    | Anna      | 123456789 |  32 | Senior Software Engineer_        |
    |-----------|-----------|-----|----------------------------------|
    | Alexander | 987654321 |  28 | Senior Software Testing Engineer |
    |           |           |     |                                  |

Then let's move cursor to before word *Engineer* in the first row and click *alt+enter*

    |    Name   |   Phone   | Age |             Position             |
    |-----------|-----------|-----|----------------------------------|
    | Anna      | 123456789 |  32 | Senior Software                  |
    |           |           |     | Engineer_                        |
    |-----------|-----------|-----|----------------------------------|
    | Alexander | 987654321 |  28 | Senior Software Testing Engineer |
    |           |           |     |                                  |

Move cursor before word *Software* in the first row and click *alt+enter* again

    |    Name   |   Phone   | Age |             Position             |
    |-----------|-----------|-----|----------------------------------|
    | Anna      | 123456789 |  32 | Senior                           |
    |           |           |     | Software Engineer_               |
    |-----------|-----------|-----|----------------------------------|
    | Alexander | 987654321 |  28 | Senior Software Testing Engineer |
    |           |           |     |                                  |

Move cursor to the first row after word *Senior* and click *ctrl+j*

    |    Name   |   Phone   | Age |             Position             |
    |-----------|-----------|-----|----------------------------------|
    | Anna      | 123456789 |  32 | Senior Software Engineer_        |
    |-----------|-----------|-----|----------------------------------|
    | Alexander | 987654321 |  28 | Senior Software Testing Engineer |
    |           |           |     |                                  |

Let's move cursor with tab key to second row(hlines skipped automatically) and click *ctrl+k,enter*

    |    Name   |   Phone   | Age |             Position             |
    |-----------|-----------|-----|----------------------------------|
    | Anna      | 123456789 |  32 | Senior Software Engineer         |
    |-----------|-----------|-----|----------------------------------|
    | Alexander | 987654321 |  28 | Senior Software Testing Engineer |
    |-----------|-----------|-----|----------------------------------|
    | _         |           |     |                                  |


### Convert CSV into table

Select some text with CSV data

    Name,Age
    Anna,20
    Alexander,27

 and then click *ctrl+k, |* to convert CSV data into table, you will get

    | Name      | Age |
    | Anna      | 20  |
    | Alexander | 27  |

*Convert CSV into table* command automatically recognize CSV dialect, for example you can enter data separated by *tab*. If *Convert CSV into table* command can not recognize CSV dialect you will get one row table where selected line is a row in the table.


### Temporary Enable/Disable *Table Editor* for current view

Some time you like temporary enable table editor and then disable it. It is useful if you edit *Python* or *Java* code and like to pretty print table, then continue edit your code.
For do this you should:

* Click *ctrl+shift+p* for show command palette
* Select *Table Editor: Enable for current view*

Then after you edit table you can disable Table Editor

* Click *ctrl+shift+p* for show command palette
* Select *Table Editor: Disable for current view*

### Switch table syntax on the fly

Table editor support next table syntax:

- Simple
- EmacsOrgMode
- Pandoc
- Multi Markdown
- reStructuredText
- Textile

Table Editor syntax detected by user settings and if it is not specified recognized automatically by view syntax. But you can change table syntax on the fly with command palette:

- Table Editor: Set table syntax 'Simple' for current view
- Table Editor: Set table syntax 'EmacsOrgMode' for current view
- Table Editor: Set table syntax 'Pandoc' for current view
- Table Editor: Set table syntax 'MultiMarkdown' for current view
- Table Editor: Set table syntax 'reStructuredText' for current view
- Table Editor: Set table syntax 'Textile' for current view

Above commands automatically enable table editor for current view.

### Demo 

Press *ctrl+shift+p* to launch command palette and select *Table Editor: Show demo film in new scratch view*. It is integration test and demo at the same time. 


## Installation


### Using Sublime Package Control

It is preferred and simplest way for most users. 

- Install Package Control http://wbond.net/sublime_packages/package_control
- Open Package Control
- Select *Install Package*
- Find and select *Table Editor*

### Using Git

If you like work with HEAD you can locate *Table Editor* in your packages directory.

- Go to your Packages directory, you can locate to your Packages directory by using the menu item 
  *Preferences -> Browse Packages...*
- Inside the Packages directory, clone the SublimeTableEditor repository using the command below: 

  *git clone https://github.com/vkocubinsky/SublimeTableEditor.git "Table Editor"*
  

### Download Manually

- Download the files using the GitHub .zip download option.
- Unzip the files and rename the folder to *Table Editor*.
- Copy the folder to your Sublime Text 2 Packages directory.

## Settings

You can customize *Table Editor* by change settings. For do this you have to modify settings file (see http://docs.sublimetext.info/en/latest/customization/settings.html).

For apply changes for all files you can open user settings with menu "Preferences -> Settings - User". For apply changes for specific syntax you can open syntax settings with menu "Preferences -> Settings - More -> Syntax Specific - User". 

### Enable Table Editor

By default *Table Editor* is disabled. For enable *Table Editor* you have to set

```javascript
{
    "enable_table_editor": true
}
```

Usually you will enable *Table Editor* for specific syntax.
You can do this very easy if launch command palette by *ctrl+shift+p* and select 
*Table Editor: Enable for current syntax*. 


### Set Table Syntax

You can control table syntax with settings

```javascript
{
    // Set table syntax for Table Editor.
    // Valid options are: "Auto", "Simple", "EmacsOrgMode", "Pandoc", "MultiMarkdown",
    //                    "reStructuredText", "Textile"
    "table_editor_syntax": "Auto"
}
```

"Auto" settings detect table syntax by view syntax with next rules:

- Markdown, MultiMarkdown -> MultiMarkdown
- reStructuredText -> reStructuredText
- Textile -> Textile
- Other -> Simple


### Override Table Border Style

Table Border Style is a part of Table Syntax and usually we should not change this. But if you like you can override table border style. Table editor supports next table border styles:

* simple: *|---|---|*
* emacs: org mode *|---+---|*
* grid: *+---+---+* 

```javascript
{
    // Override border style for Table Editor
    // Valid options are: "simple", "grid", "emacs"
    "table_editor_border_style": "simple"

    // Deprecated 
    // see "table_editor_border_style"
    // "table_editor_style": "simple"
}
```

### Override custom column alignment

This settings by default enabled only for Simple Table Syntax, but you enable it for other syntax

```javascript
{
    // If table_editor_custom_column_alignment is true, supports '<', '>', '#' column alignment
    "table_editor_custom_column_alignment": true
}
```

With this feature you can explicit set justification with format characters 

* '<' - left 
* '>' - right
* '#' - center

as in next example

    | column 1 | column 2 | column 3 |
    | <<<<<<<< | >>>>>>>> | ######## |
    |----------|----------|----------|
    | 1        |    row 1 |    c1    |
    | 2        |    row 2 |    c2    |
    | 3        |    row 3 |    c3    |

You can change justification several times

    | column 1 | column 2 | column 3 |
    | <<<<<<<< | >>>>>>>> | ######## |
    |----------|----------|----------|
    | 1        |    row 1 |    c1    |
    | 2        |    row 2 |    c2    |
    | 3        |    row 3 |    c3    |
    | ######## | <<<<<<<< | >>>>>>>> |
    |    1     | row 1    |       c1 |
    |    2     | row 2    |       c2 |
    |    3     | row 3    |       c3 |

### Override MultiMarkdown custom column alignment

This settings by default enabled only for MultiMarkdown Table Syntax, but you enable it for other syntax

```javascript
{
    // If table_editor_multimarkdown_alignment is true, supports ":---", ":---:","---:" 
    // column alignment
    "table_editor_multi_markdown_column_alignment": true
}
```



## Key binding

**ctrl+shift+a**

        Re-align the table without change the current table field. Move cursor to begin of the current table field.

**tab**

        Re-align the table, move to the next field. Creates a new row if necessary. 

**shift+tab**

        Re-align, move to previous field.

**enter**

        Re-align the table and move down to next row. Creates a new row if necessary.
        At the beginning or end of a line, enter still does new line.

**alt+left**

        Move the current column left.

**alt+right**

        Move the current column right.

**alt+shift+left**

        Kill the current column.

**alt+shift+right**

        Insert a new column to the left of the cursor position.

**alt+shift+up**

        Kill the current row or horizontal line.

**alt+shift+down**

        Insert a new row above the current row. 

**alt+up**

        Move current row up

**alt+down**

        Move current row down

**ctrl+k, -**

        Insert single horizontal line below current row. 

**ctrl+k, =**

        Insert double horizontal line below current row. 


**ctrl+k, enter**

        Insert a horizontal line below current row, and move the cursor into the row below that line. 

**ctrl+k, |**

        Convert selected CSV region into table

**alt+enter**
    
        Split rest of cell down from current cursor position,
        insert new line bellow if current row is last row in the table or if next line is hline

 **ctrl+j**
        
        Join current row and next row into one if next row is not hline
 

## License

Package is distributed by GNU General Public License v3.0.

## Donation

You can make a donation online, using the link below with PayPal service

[Donate](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CBL373WUKNTZW 
                "Make a Donation to the Table Editor")

## Testing

I tested *Table Editor* package under Windows and Linux. It should work under Mac, but I did not test, because I do not have a mac.


