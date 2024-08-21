unit uExibeTexto;

{$MODE Delphi}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormExibeTexto = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormExibeTexto: TFormExibeTexto;

implementation

{$R *.lfm}

procedure TFormExibeTexto.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
