package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.domain.GeneralStock;

import java.util.Date;
import java.util.List;

public interface GeneralStockRepositoryCustom {
    Page<GeneralStock> searchForGeneralStock(
            Long clientId,
            List<Long> supplierProductIds,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
