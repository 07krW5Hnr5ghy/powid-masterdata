package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Model;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;

import java.util.List;

@Repository
public interface ProductRepositoryCustom {
    Page<Product> searchForProduct(
            Long clientId,
            String sku,
            List<Long> modelIds,
            List<Long> brandIds,
            List<Long> sizeIds,
            List<Long> categoryProductIds,
            List<Long> colorIds,
            List<Long> unitIds,
            Boolean pictureFlag,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
