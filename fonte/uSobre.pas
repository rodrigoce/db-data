unit uSobre;

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
  TFormSobre = class(TForm)
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Sobre;
  end;

var
  FormSobre: TFormSobre;

implementation

{$R *.lfm}

{ TFormSobre }

class procedure TFormSobre.Sobre;
begin
  Application.CreateForm(TFormSobre, FormSobre);
  with FormSobre do
  begin
    ShowModal;
    Free;
  end;
end;

end.
