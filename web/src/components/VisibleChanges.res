let use = () => {
  let (isChecked, setToggle) = React.useState(_ => false)
  let label = switch isChecked {
  | true => "Hide hidden/masked changes"
  | false => "Reveal hidden/masked changes"
  }
  let onChange = (_, _) => setToggle(x => !x)
  let isChangeVisible = (hiddenStatus, maskedStatus) =>
    switch (isChecked, hiddenStatus, maskedStatus) {
    | (false, HiddenChanges.Hidden, _) => false
    | (false, _, MaskedChanges.Masked) => false
    | (false, Visible | Updated, Unmasked) => true
    | (true, _, _) => true
    }

  let toggleButton =
    <span style={ReactDOM.Style.make(~float="right", ())}>
      <Patternfly.Tooltip content="Set to show or hide hidden/masked changes">
        <Patternfly.Checkbox id="hidden-toggle" isChecked onChange label />
      </Patternfly.Tooltip>
    </span>

  (toggleButton, isChangeVisible)
}
