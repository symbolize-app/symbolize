import * as conveyExpression from '@/expression.ts'

export function hover<Value>(
  value: conveyExpression.ExpressionOpt<Value, conveyExpression.FullScope>,
): Pseudo<Value> {
  return new Pseudo('hover', value)
}

export class Pseudo<Value>
  implements
    conveyExpression.Expression<Value, conveyExpression.FullScope>
{
  readonly [conveyExpression.expressionMarker] = null

  constructor(
    private readonly name: string,
    private readonly value: conveyExpression.ExpressionOpt<
      Value,
      conveyExpression.FullScope
    >,
  ) {}

  compile(): string {
    throw new Error(
      `Method not implemented. ${this.name} ${typeof this.value}`,
    )
  }

  intern(): conveyExpression.Expression<
    Value,
    conveyExpression.FullScope
  > {
    throw new Error('Method not implemented.')
  }
}
