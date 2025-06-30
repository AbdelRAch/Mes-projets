import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# Function to fetch page source
def fetch_page_source(url):
    # Initialize the browser driver (Firefox in this example)
    driver = webdriver.Firefox()

    try:
        # Open the URL
        driver.get(url)
        
        # Wait for and click all toggle elements
        toggle_elements = WebDriverWait(driver, 10).until(
            EC.visibility_of_all_elements_located((By.XPATH, '//*[contains(@id, "toggle")]'))
        )
        for toggle_element in toggle_elements:
            toggle_element.click()

        # Wait until the content is fully visible
        WebDriverWait(driver, 10).until(
            EC.visibility_of_element_located((By.XPATH, '//*[contains(concat(" ", @class, " "), concat(" ", "name", " "))]'))
        )

        # Capture the page source
        page_source = driver.page_source

        # Return the page source
        return page_source

    except Exception as e:
        print(f"Failed to retrieve content from {url}. Error: {e}")
        return None

    finally:
        # Close the browser
        driver.quit()
