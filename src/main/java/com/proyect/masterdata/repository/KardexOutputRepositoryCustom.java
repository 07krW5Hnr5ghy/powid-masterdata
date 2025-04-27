package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.KardexOutput;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface KardexOutputRepositoryCustom {
    Page<KardexOutput> searchForKardexOutput(
            UUID clientId,
            Integer quantity,
            String product,
            UUID productId,
            String username,
            String warehouse,
            String model,
            String subCategoryProduct,
            String category,
            String size,
            String color,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
