package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.SupplierProduct;

import java.util.List;

@Repository
public interface SupplierProductRepositoryCustom {
    Page<SupplierProduct> searchForSupplierProduct(
            Long clientId,
            String serial,
            String productSku,
            List<Long> supplierIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
    }
