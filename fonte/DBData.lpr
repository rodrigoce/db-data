program DBData;

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uFuncoes, uVisualizarTriggers, uVariaveisGlobais, uSobre,
  uSearchRelatedTable, uPrincipal, uPesquisarTabelas, uPesquisarDiagramas,
  uObterMetaDados, uMoverObjetos, uLog, uFeaturesHandler, uExibeTexto,
  uERNotationsCore, uEntityRelationshipArrowShape, uConnection, uConfigConexao,
  uAppFile, uFrameQueryEditor, uIncludeTables
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'DB-Data';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.

