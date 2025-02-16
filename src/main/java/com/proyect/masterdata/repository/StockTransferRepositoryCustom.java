package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransfer;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransferRepositoryCustom {
    Page<StockTransfer> searchForStockTransfer(
            UUID clientId,
            List<String> serials,
            List<UUID> originWarehouseIds,
            List<UUID> destinationWarehouseIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
