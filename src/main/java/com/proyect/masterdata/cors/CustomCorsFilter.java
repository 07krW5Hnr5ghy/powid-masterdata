package com.proyect.masterdata.cors;

import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class CustomCorsFilter extends CorsFilter {

    private static final Logger logger = LoggerFactory.getLogger(CustomCorsFilter.class);

    public CustomCorsFilter(CorsConfigurationSource configSource) {
        super(configSource);
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        logger.debug("CORS Filter: Intercepting request - Method: {} | URI: {}", request.getMethod(),
                request.getRequestURI());
        super.doFilterInternal(request, response, filterChain);
    }

}
