let languages = [];
const list = document.getElementById("list");

const humanize = (text) => {
  return text
    .replace(/([A-Z])/g, " $1")
    .replace(/^./, (firstChar) => firstChar.toUpperCase());
};

const renderLang = (lang) => {
  const li = document.createElement("li");

  const examplesHTML = lang.examples?.length > 0
    ? `<button class="toggle-examples">Show Examples</button>
       <div class="examples hidden">
          ${lang.examples
            .map(
              (example) =>
                `<div class="example">
                   <h5>${humanize(example.type)}</h5>
                   <pre><code>${example.content}</code></pre>
                 </div>`
            )
            .join("")}
       </div>`
    : '';

  li.innerHTML = `<div data-name="${lang.name}">
                      <div class="header">
                          <h4>${lang.name}</h4>
                          <div class="paradigms">
                              ${lang.paradigms
                                .sort()
                                .map(
                                  (paradigm) =>
                                    `<span class="paradigm">${humanize(
                                      paradigm
                                    )}</span>`
                                )
                                .join("")}
                          </div>
                      </div>
                      <div class="first-appeared">First Appeared: ${
                        lang.yearFirstPublished
                      }</div>
                      <div class="original-authors">Original Authors: ${lang.originalAuthors.join(
                        ", "
                      )}</div>
                      <p>${lang.description}</p>
                      <div class="resources">
                          ${lang.resources
                            .map(
                              (resource) =>
                                `<a href="${resource.href}">${resource.title}</a>`
                            )
                            .join(" | ")}
                      </div>
                      ${examplesHTML}
                    </div>`;

  // Event listener for toggle button
  if (lang.examples?.length > 0) {
    const toggleButton = li.querySelector('.toggle-examples');
    const examplesDiv = li.querySelector('.examples');

    toggleButton.addEventListener('click', () => {
      const isHidden = examplesDiv.classList.contains('hidden');
      examplesDiv.classList.toggle('hidden');
      toggleButton.textContent = examplesDiv.classList.contains('hidden')
        ? 'Show Examples'
        : 'Hide Examples';

      // Apply syntax highlighting when examples are first shown
      if (isHidden && typeof hljs !== 'undefined') {
        examplesDiv.querySelectorAll('pre code').forEach((block) => {
          hljs.highlightElement(block);
        });
      }
    });
  }

  list.appendChild(li);
};

const filterLangs = () => {
  const searchInput = document.getElementById("search-input");
  const search = searchInput.value.toLowerCase();

  const selectedParadigms = Array.from(
    document.querySelectorAll("#paradigms input:checked")
  ).map((cb) => cb.id);
  const selectedTyping = Array.from(
    document.querySelectorAll("#typing input:checked")
  ).map((cb) => cb.id);

  const matchingLangs = languages.filter((lang) => {
    const matchesParadigms = selectedParadigms.every((paradigm) =>
      lang.paradigms.includes(paradigm)
    );
    const matchesTyping = selectedTyping.every((type) =>
      lang.typing.includes(type)
    );
    const matchesSearch =
      lang.name.toLowerCase().includes(search) ||
      lang.description.toLowerCase().includes(search);
    return matchesParadigms && matchesTyping && matchesSearch;
  });
  list.innerHTML = "";
  matchingLangs.forEach(renderLang);
  if (matchingLangs.length === 0) {
    list.innerHTML = "<li><div>No results</div></li>";
  }
};

document.getElementById("search-input").addEventListener("keyup", filterLangs);
document.getElementById("filters").addEventListener("change", filterLangs);

fetch("pldb.json")
  .then((response) => response.json())
  .then((langs) => {
    languages = langs;

    langs.forEach(renderLang);
  });
