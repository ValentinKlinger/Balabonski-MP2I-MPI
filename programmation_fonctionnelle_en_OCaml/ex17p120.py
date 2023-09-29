def max_kadane(liste: list[int]) -> list[int, list[int]]:
    """
    >>> max_kadane([1, 2, 3, 4])
    [10, [1, 2, 3, 4]]

    >>> max_kadane([1, 2, -4, 4])
    [4, [4]]

    >>> max_kadane([])
    [0, []]

    >>> max_kadane([-1, 2, 3, -4])
    [5, [2, 3]]
    """

    meilleur_somme: list = [0, []]
    somme_actuelle: list = [0, []]

    for valeur in liste:
        if somme_actuelle[0] + valeur <= valeur:
            somme_actuelle = [valeur, [valeur]]
        else:
            somme_actuelle[0] += valeur
            somme_actuelle[1].append(valeur)

        if meilleur_somme[0] <= somme_actuelle[0]:
            meilleur_somme = [somme_actuelle[0], somme_actuelle[1].copy()]

    return meilleur_somme
