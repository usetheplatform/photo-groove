class RangeSlider extends HTMLElement {
    connectedCallback() {
        let input = document.createElement("input");
        this.appendChild(input);

        let jsr = new JSR(input, {
            max: this.max,
            values: [this.val],
            sliders: 1,
            grid: false
        });

        jsr.addEventListener("update", (element, value) => {
            let event = new CustomEvent("slide", {
                detail: { userSlideTo: value }
            });

            this.dispatchEvent(event);
        });

    }
}

window.customElements.define("range-slider", RangeSlider);
