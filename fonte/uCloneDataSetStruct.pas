unit uCloneDataSetStruct;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 07/06/2013
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StrUtils,
  Dialogs, ExtCtrls, Menus, StdCtrls, Types, DB, {DBClient,} TypInfo;

{procedure CopyFields(SourceDataset, DestDataset: TDataset; doAdd: Boolean);
function CloneField(Source: TField; AOwner: TComponent): TField;}

implementation

{procedure CopyFields(SourceDataset, DestDataset: TDataset; doAdd: Boolean);
var
  i: integer;
  Fld: TField;
begin
  if not doAdd then DestDataset.Fields.Clear;

  for i := 0 to SourceDataset.Fields.Count -1 do
  begin
    if Assigned(DestDataset.Fields.FindField(SourceDataset.Fields[i].FieldName)) then
       Continue;
    Fld := CloneField(SourceDataset.Fields[i], DestDataset.Fields.Dataset);
    Fld.DataSet := DestDataset.Fields.Dataset;
  end;
end;

function CloneField(Source: TField; AOwner: TComponent): TField;

  procedure SetProp(Name: string);
  var
    V: variant;
    PropInfo: PPropInfo;
  begin
    PropInfo := TypInfo.GetPropInfo(Source, Name);
    if PropInfo <> nil then
      try V := TypInfo.GetPropValue(Source, Name);
        if not VarIsNull(V) then
          TypInfo.SetPropValue(Result, Name, V);
       except
         ; //just kill exception
       end;
  end;
var
  i: Integer;
const
  CloneProperty: array [0..18] of string =
    ( 'EditMask', 'FixedChar', 'Size', 'Transliterate', 'DisplayFormat'
    , 'EditFormat', 'Currency', 'MaxValue', 'MinValue', 'Precision'
    , 'DisplayValues', 'BlobType', 'ObjectType', 'IncludeObjectField'
    , 'ReferenceTableName', 'Active', 'Expression', 'GroupingLevel'
    , 'IndexName');
begin
  Result := TFieldClass(Source.ClassType).Create(AOwner);
  Result.Alignment := Source.Alignment;
  //Result.AutoGenerateValue := Source.AutoGenerateValue;
  Result.CustomConstraint := Source.CustomConstraint;
  Result.ConstraintErrorMessage := Source.ConstraintErrorMessage;
  Result.DefaultExpression := Source.DefaultExpression;
  Result.DisplayLabel := Source.DisplayLabel;
  Result.DisplayWidth := Source.DisplayWidth;
  Result.FieldKind := Source.FieldKind;
  Result.FieldName := Source.FieldName;
  Result.ImportedConstraint := Source.ImportedConstraint;
  Result.LookupDataSet := Source.LookupDataSet;
  Result.LookupKeyFields := Source.LookupKeyFields;
  Result.LookupResultField := Source.LookupResultField;
  Result.KeyFields := Source.KeyFields;
  Result.LookupCache := Source.LookupCache;
  Result.ProviderFlags := Source.ProviderFlags;
  Result.ReadOnly := False;//Source.ReadOnly;
  Result.Required := Source.Required;
  Result.Visible := Source.Visible;

  for i := Low(CloneProperty) to High(CloneProperty) do
  begin
    SetProp(CloneProperty[i]);
  end;
end;}

end.
