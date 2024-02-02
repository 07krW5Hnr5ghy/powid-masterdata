package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancelledOrder;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface CancelledOrderRepositoryCustom {
    Page<CancelledOrder> searchForCancelledOrder(Long orderId,Long clientId,String sort,String sortColumn,Integer pageNumber,Integer pageSize);
}
