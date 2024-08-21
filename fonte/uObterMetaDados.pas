unit uObterMetaDados;

{$MODE Delphi}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms, StrUtils,
  Dialogs, ExtCtrls, Menus, DB, sqldb, uConnection, BufDataset, uERNotationsCore;

type
  TTipoRelacionamento = (pai, filha);

  { TObterMetaDados }

  TObterMetaDados = class
    private
      class procedure AdicionarCamposDeFormaOrdenada(EntityOndeInserir: TEntity; fonte: TDataSet);
      class function montarHint(dataType, dataLength: string): string;
    public
      class procedure ObterChavesDaEntidade(EntityOndeInserir: TEntity);
      class procedure ObterTabelasRelacionadasFilhas(OwnerDaPK, NomeDaConstraintPK, NomeTabelaPai: string; buffer: TBufDataSet);
      class procedure ObterTabelasRelacionadasPai(Owner, Tabela: string; buffer: TBufDataSet);
      class procedure ObterTabelaEColunaRelacionada(EntityConstraint: TEntityConstraint);
      class procedure ObterCamposNaoChave(EntityOndeInserir: TEntity);
      class function TabelaExiste(Owner, Tabela: string): boolean;
      class procedure ObterListaDeOwners;
      class procedure PesquisarObjetos(owner, nomeObjeto: string; TipoPesquisa: Integer; apenasTabelas: Boolean; buffer: TBufDataSet);
      class procedure ObterTriggers(Owner, Tabela: string; buffer: TBufDataSet);
      class procedure PopularBufferComSelect(sql: string; buffer: TBufDataSet);
  end;

implementation

uses uPrincipal, uVariaveisGlobais;

{ TObterMetaDados }

// Insere os campos na entidade nós critérios:
// 1. chave primária primeiro
// 1.1 inserir segundo a position do campo
// 2. chaves estrangerias em seguinda
// 2.1 inserir as chaves estrangérias primeiro ordenando pelo column_id e depois por position
class procedure TObterMetaDados.AdicionarCamposDeFormaOrdenada(
  EntityOndeInserir: TEntity; fonte: TDataSet);


  procedure copiarEOrdenar(parDestino, parFonte: TDataSet);
  var
    i: Integer;
  begin
    while not parFonte.Eof do
    begin
      parDestino.Append;
      for i := 0 to parFonte.FieldCount - 1 do
      begin
        parDestino.Fields[i].Value := parFonte.Fields[i].Value;
      end;
      parDestino.Post;
      parFonte.Next;
    end;
  end;

  // conforme lógica do algoritmo como um todo, essa procedure inserirá uma constraint por vez na entity
  // ao seu fim o parametro "fonte" esta sempre vazio.
  procedure InserirNaEnity(fonte: TDataSet);
  var
    hint: string;
    campo: string;
    r_owner: string;
    r_constraint_name: string;
    linha: TCustomEntityColumn;
    constraint: TEntityConstraint;
  begin
    fonte.First;
    while not fonte.Eof do
    begin
      hint := montarHint(fonte.FieldByName('data_type').AsString, fonte.FieldByName('data_length').AsString);

      campo := LowerCase(fonte.FieldByName('column_name').AsString);
      linha := EntityOndeInserir.AddColunaChave(campo, TipoLinhaNomeColuna, hint);
      // adiciona a constraint na entity
      if fonte.FieldByName('CONSTRAINT_TYPE').AsString = 'P' then // chave primaria
      begin
        EntityOndeInserir.AddColunaConstraint('P', // caption
                                                  TipoLinhaPK, // tipo linha
                                                  fonte.FieldByName('CONSTRAINT_NAME').AsString, // hint
                                                  fonte.FieldByName('OWNER').AsString, // constraintOwner
                                                  fonte.FieldByName('CONSTRAINT_NAME').AsString, // constraintName
                                                  fonte.FieldByName('COLUMN_NAME').AsString, // constraintColumnName
                                                  TShape(linha).Top, // posicaoTop
                                                  '', // rOwner
                                                  ''); // rConstraintName
      end
      else // chave estrangeira
      begin
          r_owner := fonte.FieldByName('R_OWNER').AsString;
          r_constraint_name := fonte.FieldByName('R_CONSTRAINT_NAME').AsString;

        constraint := EntityOndeInserir.AddColunaConstraint('F', // caption
                                                  TipoLinhaFK, // tipo linha
                                                  fonte.FieldByName('CONSTRAINT_NAME').AsString, // hint
                                                  fonte.FieldByName('OWNER').AsString, // constraintOwner
                                                  fonte.FieldByName('CONSTRAINT_NAME').AsString, // constraintName
                                                  fonte.FieldByName('COLUMN_NAME').AsString, // constraintColumnName
                                                  TShape(linha).Top, // posicaoTop
                                                  r_owner, // rOwner
                                                  r_constraint_name); // rConstraintName
        // termina de obter as propriedades da constraint FK
        ObterTabelaEColunaRelacionada(constraint);
      end;
      fonte.Delete;
    end;
  end;

var
  temp, temp2: TBufDataSet;
begin
  temp := TBufDataSet.Create(nil);
  //clona a estrututa de dados da tabela fonte criando seus fields
  temp.CopyFromDataset(fonte, False); //CopyFields(fonte, temp, False); temp.CreateDataSet;
  // indexa por position para inserir na mesna ordem da constraint
  temp.IndexFieldNames := 'POSITION';
  // filtra a chave primária
  fonte.Filter := 'CONSTRAINT_TYPE = ''P''';
  fonte.Filtered := True;
  //define o nome da constraint da chave primária
  EntityOndeInserir.PrimaryKeyConstraintName := fonte.FieldByName('CONSTRAINT_NAME').AsString;
  // copia a chave primaria ordenando pos position
  copiarEOrdenar(temp, fonte);
  // inserir os campos da chave primaria na entity
  InserirNaEnity(temp);
  // filtra as chaves estrageiras
  fonte.Filter := 'CONSTRAINT_TYPE = ''R''';
  // copia as chaves estrangeiras ordenando por column id
  temp.IndexFieldNames := 'COLUMN_ID';
  copiarEOrdenar(temp, fonte);
  // agora que está ordenado por column id, copiar para outra cds ordenando por position
  temp2 := TBufDataSet.Create(nil);
  // clona estrututa de dados
  temp2.CopyFromDataset(fonte, False); //CopyFields(fonte, temp2, False); temp2.CreateDataSet;
  // ordena por position
  temp2.IndexFieldNames := 'POSITION';
  temp.First;
  //enquanto tiver registos
  while not temp.Eof do
  begin
    // filtra pelo nome das contraint para pegar todos os campos dessa constraint
    temp.Filter := 'CONSTRAINT_NAME = ' + QuotedStr(temp.FieldByName('CONSTRAINT_NAME').AsString);
    temp.Filtered := True;
    copiarEOrdenar(temp2, temp);
    InserirNaEnity(temp2);
    temp.First;
    while not temp.Eof do
      temp.Delete; // elinita o registro ja copiado para nao copiar mais
    temp.Filtered := False;
    temp.First;
  end;
  temp2.Close;
  temp2.Free;
  temp.Close;
  temp.Free;
end;

class function TObterMetaDados.montarHint(dataType, dataLength: string): string;
begin
  if (dataType = 'VARCHAR2') or
     (dataType = 'CHAR') or
     (dataType = 'NUMBER') or
     (dataType = 'VARCHAR') then
    Result := dataType + '(' + dataLength + ')'
  else
    Result := dataType;
end;

class procedure TObterMetaDados.ObterCamposNaoChave(
  EntityOndeInserir: TEntity);
var
  q: TSQLQuery;
  i: Integer;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;

  q.SQL.Add('select column_name, data_type, data_length from all_tab_columns where owner = :owner and table_name = :tabela');

  q.SQL.Add('and column_name not in (');
  for i := 0 to EntityOndeInserir.ListColunasChave.Count - 1 do
    q.SQL.Add(QuotedStr(UpperCase(EntityOndeInserir.ListColunasChave[i])) + IfThen(i <> EntityOndeInserir.ListColunasChave.Count - 1, ', ', ''));

  q.SQL.Add(') order by column_id');

  q.Params.ParamByName('owner').Value := EntityOndeInserir.SchemaOwner;
  q.Params.ParamByName('tabela').Value := EntityOndeInserir.NomeTabela;
  q.Open;

  while not q.Eof do
  begin
    EntityOndeInserir.AddColunaNaoChave(LowerCase(q.FieldByName('column_name').AsString),
                                        TipoLinhaNaoChave,
                                        montarHint(q.FieldByName('data_type').AsString,
                                                   q.FieldByName('data_length').AsString));
    q.Next;
  end;

  EntityOndeInserir.ExibindoTodosOsCampos := True;

  q.Close;
  q.Free;
end;

class procedure TObterMetaDados.ObterChavesDaEntidade(EntityOndeInserir: TEntity);
var
  q: TSQLQuery;
begin
  FormPrincipal.SetarAtividadeStatusPanelBar('Obtendo metada de ' + EntityOndeInserir.SchemaOwner + '.' + EntityOndeInserir.NomeTabela);
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;
  q.SQL.Add('select b.CONSTRAINT_TYPE, b.CONSTRAINT_NAME, b.OWNER, c.POSITION, a.COLUMN_NAME, a.DATA_TYPE, a.COLUMN_ID, b.R_OWNER, b.R_CONSTRAINT_NAME, a.DATA_LENGTH ');
  q.SQL.Add('from all_tab_columns a ');
  q.SQL.Add('join all_constraints b on a.OWNER = b.OWNER and a.TABLE_NAME = b.TABLE_NAME ');
  q.SQL.Add('join all_cons_columns c on a.OWNER = c.OWNER and b.CONSTRAINT_NAME = c.CONSTRAINT_NAME and a.TABLE_NAME = c.TABLE_NAME and a.COLUMN_NAME = c.COLUMN_NAME ');
  q.SQL.Add('where a.owner = :owner and b.CONSTRAINT_TYPE in (''P'', ''R'') and a.TABLE_NAME = :tabela ');
  q.SQL.Add('order by b.CONSTRAINT_TYPE, a.COLUMN_ID, c.POSITION ');
  q.Params[0].Value := EntityOndeInserir.SchemaOwner;
  q.Params[1].Value := EntityOndeInserir.NomeTabela;
  q.Open;
  FormPrincipal.SetarAtividadeStatusPanelBar('Renderizando...');
  AdicionarCamposDeFormaOrdenada(EntityOndeInserir, q);
  q.Close;
  q.Free;
  FormPrincipal.SetarAtividadeStatusPanelBar('');
end;

class procedure TObterMetaDados.ObterListaDeOwners;
var
  q: TSQLQuery;
begin
  if Assigned(CacheDeOwners) then
    CacheDeOwners.Clear
  else
    CacheDeOwners := TStringList.Create;

  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;
  q.SQL.Add('select distinct owner from all_tables order by owner');
  q.Open;

  while not q.Eof do
  begin
    CacheDeOwners.Add(q.FieldByName('owner').AsString);
    q.Next;
  end;
  q.Close;
  q.Free;
end;

// não usa para o MySQL
class procedure TObterMetaDados.ObterTabelaEColunaRelacionada(EntityConstraint: TEntityConstraint);
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;
  q.SQL.Add('select owner, table_name, column_name from all_cons_columns where constraint_name = :constraintName AND owner = :owner');
  q.Params[0].Value := EntityConstraint.RConstraintName;
  q.Params[1].Value := EntityConstraint.ROwner;
  q.Open;

  EntityConstraint.TabelaRelacionada := q.FieldByName('table_name').AsString;
  EntityConstraint.ColunaRelacionada := q.FieldByName('column_name').AsString;
  EntityConstraint.OwnerTabelaRelacionada := q.FieldByName('owner').AsString;
  q.Close;
  q.Free;
end;

class procedure TObterMetaDados.ObterTabelasRelacionadasFilhas(
  OwnerDaPK, NomeDaConstraintPK, NomeTabelaPai: string; buffer: TBufDataSet);
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;

  q.SQL.Add('select distinct Lower(owner) Owner, Lower(table_name) Tabela from all_constraints where r_owner = :owner and r_constraint_name = :constraint and constraint_type = ''R''');

  q.Params[0].Value := OwnerDaPK;

  q.Params[1].Value := NomeDaConstraintPK;

  q.Open;
  buffer.CopyFromDataset(q);
  buffer.First;

  q.Free;
end;

class procedure TObterMetaDados.ObterTabelasRelacionadasPai(Owner,
  Tabela: string;buffer: TBufDataSet);
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;

  q.SQL.Add('select distinct Lower(b.owner) Owner, Lower(b.table_name) Tabela from all_constraints a ');
  q.SQL.Add('join all_constraints b on a.r_owner = b.owner and a.r_constraint_name = b.constraint_name and b.constraint_type = ''P''');
  q.SQL.Add('where a.owner = :owner and a.TABLE_NAME = :tabela and a.constraint_type = ''R''');

  q.Params[0].Value := Owner;
  q.Params[1].Value := Tabela;

  q.Open;
  buffer.CopyFromDataset(q);
  buffer.First;

  q.Free;
end;

class procedure TObterMetaDados.ObterTriggers(Owner, Tabela: string;
  buffer: TBufDataSet);
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;

  q.SQL.Add('select lower(a.owner) owner, lower(a.trigger_name) trigger_name, lower(a.trigger_type) trigger_type, lower(a.triggering_event) triggering_event, lower(a.status) status, a.description, a.trigger_body');
  q.SQL.Add('from all_triggers a where a.owner = :owner and a.table_name = :tabela');
  q.Params[0].Value := Owner;
  q.Params[1].Value := Tabela;

  q.Open;
  buffer.CopyFromDataset(q);
  buffer.First;

  q.Free;
end;

class procedure TObterMetaDados.PesquisarObjetos(owner, nomeObjeto: string;
  TipoPesquisa: Integer;
  apenasTabelas: Boolean;
  buffer: TBufDataSet);
var
  q: TSQLQuery;
  where: string;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;

  q.SQL.Add('select a.owner');

  if apenasTabelas then
    q.SQL.Add(', a.table_name object_name, ''TABLE'' object_type from all_tables a')
  else
    q.SQL.Add(', a.object_name, object_type from all_objects a');
  // tipo de pesquisa:
  // 0 - nome exato
  // 1 - começa com
  // 2 - termina com
  // 3 - contém
  case TipoPesquisa of
    0: where := 'where a.table_name = :objeto';
    1:
    begin
      where := 'where a.table_name like :objeto';
      nomeObjeto := nomeObjeto + '%';
    end;
    2:
    begin
      where := 'where a.table_name like :objeto';
      nomeObjeto := '%' + nomeObjeto;
    end;
    3:
    begin
      where := 'where a.table_name like :objeto';
      nomeObjeto := '%' + nomeObjeto + '%';
    end;
  end;

  // troca o campo do where
  if (not apenasTabelas) then
    where := StringReplace(where, 'a.table_name', 'a.object_name', [rfReplaceAll]);

  q.SQL.Add(where);
  //

  if Trim(owner) <> '' then
    q.SQL.Add('and a.owner = :owner');

  q.SQL.Add('order by 2');

  q.Params.ParamByName('objeto').Value := UpperCase(nomeObjeto);
  if Trim(owner) <> '' then
    q.Params.ParamByName('owner').Value := UpperCase(Owner);

  q.Open;
  buffer.CopyFromDataset(q);
  buffer.First;

  q.Free;
end;

class procedure TObterMetaDados.PopularBufferComSelect(sql: string;
  buffer: TBufDataSet);
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;
  q.PacketRecords := 15;

  q.SQL.Add(sql);

  q.Open;
  buffer.CopyFromDataset(q);
  buffer.First;

  q.Free;
end;

class function TObterMetaDados.TabelaExiste(Owner, Tabela: string): boolean;
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  q.DataBase := TDBConnection.GetCnn;

  q.SQL.Add('select 1 from all_tables where owner = :owner and table_name = :tabela');

  q.Params[0].Value := Owner;
  q.Params[1].Value := Tabela;
  q.Open;
  Result := not q.IsEmpty;
  q.Close;
  q.Free;
end;

end.
