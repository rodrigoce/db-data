program DBData;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, uFuncoes, uVisualizarTriggers, uVariaveisGlobais, uSobre,
  uSearchRelatedTable, uPrincipal, uPesquisarTabelas, uPesquisarDiagramas,
  uObterMetaDados, Unit2, uMoverObjetos, uLog, uFeaturesHandler, uExibeTexto,
  uERNotationsCore, uEntityRelationshipArrowShape, uConnection, uConfigConexao,
  uAppFile, unit3, uFrameQueryEditor
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

