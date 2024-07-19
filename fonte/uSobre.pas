unit uSobre;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 30/10/2013
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
