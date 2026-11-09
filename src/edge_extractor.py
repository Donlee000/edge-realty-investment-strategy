"""Beginner-friendly Edge Realty project extractor (Python 3.10+).

VS Code terminal:
    python -m pip install requests beautifulsoup4
    python edge_extractor.py

If the website blocks Requests, open the page normally in your browser,
save it with Ctrl+S as ready_projects.html, then run:
    python edge_extractor.py --html ready_projects.html

For several manually saved listing pages:
    python edge_extractor.py --html page1.html page2.html

Output: edge_projects.csv (UTF-8, suitable for Excel and Power BI).
This extracts ONE ROW PER PROJECT, not individual available units.

IMPORTANT: Live access returned Cloudflare HTTP 403 during development.
The label-based parsing below is a STARTER based on the supplied screenshot,
not a verified live-site integration. Inspect output before using it.
Only the supplied page(s) are processed; there is no automatic pagination
or detail-page crawling. Save additional pages and pass them with --html.
"""

import argparse
import csv
import hashlib
import re
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urljoin, urlparse

import requests
from bs4 import BeautifulSoup


SOURCE_URL = "https://www.edgerealty.ae/en/dubai-ready-projects"
FIELDS = [
    "project_id", "project_name", "location", "developer", "property_type",
    "project_url", "source_page_url", "source_file", "extracted_at_utc",
    "evidence_type", "validation_status", "raw_card_text",
]


def clean(text):
    """Collapse repeated spaces without changing the words."""
    return re.sub(r"\s+", " ", text).strip()


def get_html(url):
    """Fetch one public page. Stop on blocking; do not bypass it."""
    response = requests.get(
        url,
        headers={"User-Agent": "PropertyResearchExtractor/1.0"},
        timeout=30,
    )
    if response.status_code in (403, 429):
        raise RuntimeError(
            f"Website returned HTTP {response.status_code}. "
            "Use a normally saved browser HTML file with --html, "
            "or request an approved data export from the website."
        )
    response.raise_for_status()
    if "html" not in response.headers.get("Content-Type", "").lower():
        raise RuntimeError("The response is not an HTML page.")
    return response.content


def label_value(lines, label):
    """Read a label whether its value is on the same or the next line."""
    for index, line in enumerate(lines):
        match = re.match(rf"^{re.escape(label)}\s*:\s*(.*)$", line, re.I)
        if match:
            if match.group(1):
                return match.group(1)
            if index + 1 < len(lines):
                candidate = lines[index + 1]
                if not re.search(r"Developer\s*:|Project Type\s*:|Request Details|WhatsApp", candidate, re.I):
                    return candidate
    return ""


def find_cards(soup):
    """Find the smallest containers with a heading and both visible labels.

    Once actual HTML is available, this can be replaced with a verified
    selector, e.g. soup.select('...actual project card class...').
    Never guess a class name and treat it as confirmed.
    """
    cards = []
    for heading in soup.select("h2, h3, h4, h5, h6"):
        for parent in heading.parents:
            if parent.name in ("body", "html", "[document]"):
                break
            text = parent.get_text(" ", strip=True)
            if re.search(r"Developer\s*:", text, re.I) and re.search(r"Project Type\s*:", text, re.I):
                # A container spanning multiple projects must not become a row.
                if len(re.findall(r"Developer\s*:", text, re.I)) == 1:
                    cards.append((heading, parent))
                break
    return cards


def extract_projects(html, source_url, source_file=""):
    soup = BeautifulSoup(html, "html.parser")
    title = soup.title.get_text() if soup.title else ""
    if re.search(r"Attention Required|Just a moment|Access Denied", title, re.I):
        raise RuntimeError("This is a blocking page, not project data.")
    for element in soup.select("script, style, noscript"):
        element.decompose()

    extracted_at = datetime.now(timezone.utc).isoformat(timespec="seconds")
    rows = []
    for heading, card in find_cards(soup):
        name = clean(heading.get_text(" ", strip=True))
        lines = [clean(line) for line in card.stripped_strings if clean(line)]
        developer = label_value(lines, "Developer")
        property_type = label_value(lines, "Project Type")
        location = label_value(lines, "Location")
        if not location:
            # Screenshot order: project title, location, developer, project type.
            # Only infer a location when exactly one line lies in that position.
            if name in lines:
                start = lines.index(name) + 1
                end = next((i for i in range(start, len(lines))
                            if re.match(r"Developer\s*:", lines[i], re.I)), start)
                between = lines[start:end]
                if len(between) == 1:
                    location = between[0]

        project_url = ""
        for anchor in card.select("a[href]"):
            candidate = urljoin(source_url, anchor["href"])
            parsed = urlparse(candidate)
            if (parsed.hostname == urlparse(source_url).hostname
                    and re.search(r"/en/dubai-(?:ready|new)-projects/[^/]+", parsed.path)):
                project_url = candidate.split("#")[0]
                break

        identity = "|".join([name, location, developer]).casefold()
        rows.append({
            "project_id": hashlib.sha256(identity.encode()).hexdigest()[:16],
            "project_name": name,
            "location": location,
            "developer": developer,
            "property_type": property_type,
            "project_url": project_url,
            "source_page_url": source_url,
            "source_file": source_file,
            "extracted_at_utc": extracted_at,
            "evidence_type": "website_listing_unverified",
            "validation_status": "manual_review_required",
            "raw_card_text": " | ".join(lines),
        })
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--html", nargs="+", type=Path, help="Saved HTML listing pages")
    parser.add_argument("--url", default=SOURCE_URL, help="Listing URL / source attribution")
    parser.add_argument("--output", type=Path, default=Path("edge_projects.csv"))
    args = parser.parse_args()
    try:
        rows = []
        if args.html:
            for file in args.html:
                rows.extend(extract_projects(file.read_bytes(), args.url, file.name))
        else:
            rows = extract_projects(get_html(args.url), args.url)

        # Repeated cards across pages do not become additional investments.
        unique_rows = list({row["project_id"]: row for row in rows}.values())
        if not unique_rows:
            raise RuntimeError(
                "No project cards matched. No CSV was written. "
                "The HTML may be incomplete or the parsing rules need adjusting. "
                "Check the saved HTML for Developer and Project Type labels."
            )
        args.output.parent.mkdir(parents=True, exist_ok=True)
        with args.output.open("w", newline="", encoding="utf-8-sig") as file:
            writer = csv.DictWriter(file, fieldnames=FIELDS)
            writer.writeheader()
            writer.writerows(unique_rows)
        print(f"Saved {len(unique_rows)} candidate projects to {args.output.resolve()}")
        print("Review every field against the website before using this as evidence.")
        print("Extraction time is NOT the listing date or a historical 2026 price date.")
    except (requests.RequestException, OSError, RuntimeError) as error:
        parser.exit(1, f"Extraction stopped: {error}\n")


if __name__ == "__main__":
    main()
