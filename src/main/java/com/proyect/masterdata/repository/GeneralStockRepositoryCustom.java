package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.domain.GeneralStock;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

public interface GeneralStockRepositoryCustom {
    Page<GeneralStock> searchForGeneralStock(
            UUID clientId,
            String model,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
