package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransfer;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StockTransferRepositoryCustom {
    Page<StockTransfer> searchForStockTransfer(
            Long clientId,
            Long originWarehouseId,
            Long destinationWarehouseId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
