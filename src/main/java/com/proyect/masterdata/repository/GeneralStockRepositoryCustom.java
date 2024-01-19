package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.domain.GeneralStock;

public interface GeneralStockRepositoryCustom {
    Page<GeneralStock> searchForGeneralStock(
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
