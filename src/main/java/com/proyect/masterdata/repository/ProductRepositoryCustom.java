package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Product;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductRepositoryCustom {
    Page<Product> searchForProduct(
            UUID clientId,
            String sku,
            String model,
            List<UUID> brandIds,
            List<UUID> sizeIds,
            List<UUID> categoryProductIds,
            List<UUID> colorIds,
            List<UUID> unitIds,
            Boolean pictureFlag,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
