unit uEntityRelationshipArrowShape;

{$MODE Delphi}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Math, ExtCtrls, uERNotationsCore;

type
  TEntityRelationshipArrowShape = class(TShape)
    private
      FSentidoSeta: TSentidoSeta;
      procedure SetPosicaoSeta(Value: TSentidoSeta);
    protected
      procedure Paint; override;
    public
      property PosicaoSeta: TSentidoSeta read FSentidoSeta write SetPosicaoSeta;
  end;

implementation

{ TEntityRelationshipArrowShape }

procedure TEntityRelationshipArrowShape.Paint;
var
  p: TPoint;
begin
  Pen.Width := 1;
  Pen.Style := psSolid;
  Canvas.Pen.Color := clGray;

  if FSentidoSeta = psCima then
  begin
    p.X := 0;
    p.Y := 3;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(3, 0);

    p.X := 6;
    p.Y := 3;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(3, 0);
  end
  else if FSentidoSeta = psBaixo then
  begin
    p.X := 0;
    p.Y := 0;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(3, 3);

    p.X := 6;
    p.Y := 0;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(3, 3);
  end
  else if FSentidoSeta = psDireita then
  begin
    p.X := 0;
    p.Y := 0;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(3, 3);

    p.X := 0;
    p.Y := 6;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(3, 3);
  end
  else
  begin
    p.X := 3;
    p.Y := 0;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(0, 3);

    p.X := 3;
    p.Y := 6;
    Canvas.PenPos := p;
    //            X  Y
    Canvas.LineTo(0, 3);
  end;
end;

procedure TEntityRelationshipArrowShape.SetPosicaoSeta(Value: TSentidoSeta);
begin
  FSentidoSeta := Value;

  if PosicaoSeta in [psCima, psBaixo] then
  begin
    Width := 7;
    Height := 4;
  end
  else
  begin
    Width := 4;
    Height := 7;
  end;
end;

end.
