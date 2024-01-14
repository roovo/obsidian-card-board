import { Filter } from "./types"

export class Scrollable {
  private SCROLL_MARGIN   = 30;
  private MAX_STEP        = 20;

  private element: HTMLElement;
  private elementRect: DOMRect;

  private leftEdgeDistance: number;
  private rightEdgeDistance: number;
  private topEdgeDistance: number;
  private bottomEdgeDistance: number;

  private isInLeftEdge: boolean;
  private isInRightEdge: boolean;
  private isInTopEdge: boolean;
  private isInBottomEdge: boolean;

  constructor(element: HTMLElement, event: MouseEvent) {
    this.element      = element;
    this.elementRect  = element.getBoundingClientRect();

    const bottom      = this.elementRect.y + this.elementRect.height;
    const right       = this.elementRect.x + this.elementRect.width;

    this.leftEdgeDistance    = Math.max(0, event.clientX - this.elementRect.x);
    this.rightEdgeDistance   = Math.max(0, right - event.clientX);
    this.topEdgeDistance     = Math.max(0, event.clientY - this.elementRect.y);
    this.bottomEdgeDistance  = Math.max(0, bottom - event.clientY);

    this.isInLeftEdge    = this.leftEdgeDistance < this.SCROLL_MARGIN;
    this.isInRightEdge   = this.rightEdgeDistance < this.SCROLL_MARGIN;
    this.isInTopEdge     = this.topEdgeDistance < this.SCROLL_MARGIN;
    this.isInBottomEdge  = this.bottomEdgeDistance < this.SCROLL_MARGIN;

  }

  isInScrollableEdge(): boolean {
    return (this.isInLeftEdge || this.isInRightEdge || this.isInTopEdge || this.isInBottomEdge)
  }

  doScroll(): boolean {
    const currentScrollX  = this.element.scrollLeft;
    const currentScrollY  = this.element.scrollTop;

    const maxScrollX      = this.element.scrollWidth  - this.element.clientWidth;
    const maxScrollY      = this.element.scrollHeight - this.element.clientHeight;

    const canScrollLeft   = currentScrollX > 0;
    const canScrollRight  = currentScrollX < maxScrollX;
    const canScrollUp     = currentScrollY > 0;
    const canScrollDown   = currentScrollY < maxScrollY;

    var nextScrollX = currentScrollX;
    var nextScrollY = currentScrollY;

    if (this.isInLeftEdge && canScrollLeft) {
      var intensity = (this.SCROLL_MARGIN - this.leftEdgeDistance) / this.SCROLL_MARGIN;
      nextScrollX -= (this.MAX_STEP * intensity);
    } else if (this.isInRightEdge && canScrollRight) {
      var intensity = (this.SCROLL_MARGIN - this.rightEdgeDistance) / this.SCROLL_MARGIN;
      nextScrollX += (this.MAX_STEP * intensity);
    }

    if (this.isInTopEdge && canScrollUp) {
      var intensity = (this.SCROLL_MARGIN - this.topEdgeDistance) / this.SCROLL_MARGIN;
      nextScrollY -= (this.MAX_STEP * intensity);
    } else if (this.isInBottomEdge && canScrollDown) {
      var intensity = (this.SCROLL_MARGIN - this.bottomEdgeDistance) / this.SCROLL_MARGIN;
      nextScrollY += (this.MAX_STEP * intensity);
    }

    nextScrollX = Math.max(0, Math.min(maxScrollX, nextScrollX));
    nextScrollY = Math.max(0, Math.min(maxScrollY, nextScrollY));

    if ((nextScrollX != currentScrollX) || (nextScrollY != currentScrollY)) {
      this.element.scrollLeft = nextScrollX;
      this.element.scrollTop  = nextScrollY;

      return true;
    }
    return false;
  }
}
