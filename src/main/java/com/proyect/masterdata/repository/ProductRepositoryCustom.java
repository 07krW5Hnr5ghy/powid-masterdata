package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Model;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;

@Repository
public interface ProductRepositoryCustom {
    public Page<Product> searchForProduct(
            String sku,
            Model model,
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
