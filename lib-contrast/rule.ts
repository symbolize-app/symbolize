export function rule(
  className: string,
  code: IterableIterator<string>,
): Rule {
  return new Rule(className, ''.concat('.', className, '{', ...code, '}'))
}

export class Rule {
  constructor(
    readonly className: string,
    readonly code: string,
  ) {}
}
