unit uFrameConsultaDados;

{
Criado por: Rodrigo Castro Eleotério
Data: 07/11/2014
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, DBGrids, StdCtrls, DB, BufDataset, uERNotationsCore,
  Math;

type

  { TFrameConsultaDados }

  TFrameConsultaDados = class(TFrame)
    cds: TBufDataset;
    memoSQL: TMemo;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    btFechar: TButton;
    Ds: TDataSource;
    btAtualizar: TButton;
    procedure cdsAfterOpen(DataSet: TDataSet);
    procedure btFecharClick(Sender: TObject);
    procedure btAtualizarClick(Sender: TObject);
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

  if IniFile.ReadInteger('conexao', 'banco', 0) = 0 then // oracle
    passou := Pos('rownum', memoSQL.Lines.Text) > 0
  else
    passou := True;

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

procedure TFrameConsultaDados.btFecharClick(Sender: TObject);
begin
  FormPrincipal.DiagramaManager.RemoveContainerDaListaCarregados(FOwnerTabela);
  Self.Free;
end;

procedure TFrameConsultaDados.cdsAfterOpen(DataSet: TDataSet);
var
   ColWidth : array of cardinal;
   Counter, avaliacoes, qtdeMax  : integer;
begin
  SetLength(ColWidth,cds.FieldCount);

  for Counter := 0 to cds.FieldCount - 1 do
  begin
    ColWidth[Counter] := Max(ColWidth[Counter],DBGrid1.Canvas.TextWidth(DBGrid1.Columns[Counter].Title.Caption) + 8);
    DBGrid1.Columns[Counter].Width := ColWidth[Counter];
  end;

  try
    with cds do
    begin
      DisableControls;
      avaliacoes := 0;
      qtdeMax := Min(50, RecordCount);

      while (avaliacoes <= qtdeMax) do
      begin
        for Counter := 0 to ( FieldCount - 1 ) do
        begin
          ColWidth[Counter] := Max(ColWidth[Counter],DBGrid1.Canvas.TextWidth(Fields[Counter].AsString) + 8);
          DBGrid1.Columns[Counter].Width := ColWidth[Counter];
        end;
        Inc(avaliacoes);
        Next;
      end;
      First;
      EnableControls;
    end;
  finally
    Finalize(ColWidth);
  end;
end;

procedure TFrameConsultaDados.ObterAmostra(OwnerTabela: string);
begin
  FOwnerTabela := OwnerTabela;
  if IniFile.ReadInteger('conexao', 'banco', 0) = 0 then // oracle
    memoSQL.Lines.Add('select * from ' + OwnerTabela + ' where rownum <= 200')
  else
    memoSQL.Lines.Add('select * from ' + OwnerTabela);
  AvaliarEExecutarQuery;
end;

end.



