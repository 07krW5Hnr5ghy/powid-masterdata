package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransfer;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockTransferRepositoryCustom {
    Page<StockTransfer> searchForStockTransfer(
            Long clientId,
            List<String> serials,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
