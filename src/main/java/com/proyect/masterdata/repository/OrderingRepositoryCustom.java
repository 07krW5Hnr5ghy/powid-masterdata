package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface OrderingRepositoryCustom {
    Page<Ordering> searchForOrdering(
            Long orderId,
            Long clientId,
            String seller,
            String customer,
            String customerPhone,
            String instagram,
            List<Long> departmentIds,
            List<Long> provinceIds,
            List<Long> districtIds,
            List<Long> saleChannelIds,
            Boolean receiptFlag,
            Boolean deliveryFlag,
            List<Long> deliveryPointIds,
            List<Long> orderStateIds,
            List<Long> courierIds,
            Long paymentStateId,
            Long paymentMethodId,
            Long managementTypeId,
            Long storeId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
