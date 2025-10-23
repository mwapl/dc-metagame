import streamlit as st
import pandas as pd

st.set_page_config(page_title="DC Data Formatter", layout="centered")

st.title("📊 Duel Commander Data Formatter")
st.write("Upload your tournament results CSV, and I’ll format it for the metagame dataset!")

uploaded_file = st.file_uploader("Choose a CSV file", type="csv")

if uploaded_file:
    df = pd.read_csv(uploaded_file)
    st.write("✅ Data loaded:")
    st.dataframe(df.head())

    #TODO insert my python formatter in some way
    formatted = df

    st.download_button(
        "📥 Download formatted CSV",
        formatted.to_csv(index=False).encode("utf-8"),
        "formatted_results.csv",
        "text/csv",
    )

st.write("---")
st.write("Built by [mwapl](https://github.com/mwapl) · v1.0")
