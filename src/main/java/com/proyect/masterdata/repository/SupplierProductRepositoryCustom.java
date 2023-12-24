package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.SupplierProduct;

@Repository
public interface SupplierProductRepositoryCustom {
    public Page<SupplierProduct> searchForSupplierProduct(
            String serial,
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
