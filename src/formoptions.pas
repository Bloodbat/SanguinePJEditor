unit FormOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Buttons, ExtCtrls, ComCtrls, ActnList, Menus, StdActns;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    ActionList: TActionList;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditPaste: TEditPaste;
    lbledDefaultAuthor: TLabeledEdit;
    lbledDefaultLicense: TLabeledEdit;
    MainMenu: TMainMenu;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditPaste: TMenuItem;
    pnlControls: TPanel;
    scrlboxEditors: TScrollBox;
    Separator1: TMenuItem;
    tbCopy: TToolButton;
    tbCut: TToolButton;
    tbPaste: TToolButton;
    ToolBar: TToolBar;
    ToolButton2: TToolButton;
  end;

implementation

{$R *.lfm}

end.
