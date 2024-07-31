unit uFrameConsultaDados;

{
Criado por: Rodrigo Castro Eleotério
Data: 07/11/2014
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBGrids, StdCtrls, SynEdit, SynHighlighterSQL, DB,
  BufDataset;

type

  { TFrameConsultaDados }

  TFrameConsultaDados = class(TFrame)
    cds: TBufDataset;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    btFechar: TButton;
    Ds: TDataSource;
    btAtualizar: TButton;
    memoSQL: TSynEdit;
    sqlHighLight: TSynSQLSyn;
    procedure btFecharClick(Sender: TObject);
    procedure btAtualizarClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
    FOwnerTabela: string;
  public
    { Public declarations }
    procedure AvaliarEExecutarQuery;
    procedure ObterAmostra(OwnerTabela: string);
  end;

implementation

uses uObterMetaDados, uPrincipal, uVariaveisGlobais;

{$R *.lfm}

{ TFrameConsultaDados }

procedure TFrameConsultaDados.AvaliarEExecutarQuery;
var
   passou: Boolean;
begin
  passou := Copy(memoSQL.Lines.Text, 1, 6) = 'select';
  if not passou then
  begin
    Application.MessageBox('Por motivos de segurança a primeira palavra da instrução deve iniciar com o key word "select".'
      + #13#10 +
      'Isso porque não foi implementado controle de transações para desfazer updates e deletes.',
      'Atenção', MB_OK + MB_ICONSTOP);
  end;

  passou := Pos('rownum', memoSQL.Lines.Text) > 0;

  if not passou then
  begin
    Application.MessageBox('A key word "rownum" deve estar presente para evitar travamentos com massa de dado muito longa.',
      'Atenção', MB_OK + MB_ICONSTOP);
  end
  else
  begin
    cds.Close;
    try
      Screen.Cursor := crHourGlass;
      TObterMetaDados.PopularBufferComSelect(memoSQL.Lines.Text, cds);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TFrameConsultaDados.btAtualizarClick(Sender: TObject);
begin
  AvaliarEExecutarQuery;
end;

procedure TFrameConsultaDados.Panel1Resize(Sender: TObject);
var
  pnW: Integer;
begin
  pnW := Trunc(Panel1.Width / 2);
  btAtualizar.Left := Trunc(pnW - btAtualizar.Width + btAtualizar.Width / 2) - 2;
  btFechar.Left := Trunc(pnW + btAtualizar.Width - btAtualizar.Width / 2) + 2;
end;

procedure TFrameConsultaDados.btFecharClick(Sender: TObject);
begin
  FormPrincipal.DiagramaManager.RemoveContainerDaListaCarregados(FOwnerTabela);
  Self.Free;
end;

procedure TFrameConsultaDados.ObterAmostra(OwnerTabela: string);
begin
  FOwnerTabela := OwnerTabela;
  memoSQL.Lines.Clear;
  memoSQL.Lines.Add('select * from ' + OwnerTabela + ' where rownum <= 200');
  AvaliarEExecutarQuery;
end;

end.



