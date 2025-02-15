package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.SupplierProduct;

import java.util.List;
import java.util.UUID;

@Repository
public interface SupplierProductRepositoryCustom {
    Page<SupplierProduct> searchForSupplierProduct(
            UUID clientId,
            String serial,
            String productSku,
            String model,
            List<UUID> supplierIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
    }
