unit FormOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Buttons, ExtCtrls;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lbledDefaultAuthor: TLabeledEdit;
    lbledDefaultLicense: TLabeledEdit;
  end;

implementation

{$R *.lfm}

end.

