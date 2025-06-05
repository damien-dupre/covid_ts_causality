if FORMAT:match 'latex' then
  function Image (elem)
    elem.attributes.width = nil
    elem.attributes.height = nil
    return elem
  end
end
