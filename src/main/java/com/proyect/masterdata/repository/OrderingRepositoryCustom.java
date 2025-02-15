package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public interface OrderingRepositoryCustom {
    Page<Ordering> searchForOrdering(
            UUID orderId,
            UUID clientId,
            String seller,
            String customer,
            String customerPhone,
            String instagram,
            List<UUID> departmentIds,
            List<UUID> provinceIds,
            List<UUID> districtIds,
            List<UUID> saleChannelIds,
            Boolean receiptFlag,
            Boolean deliveryFlag,
            List<UUID> deliveryPointIds,
            List<UUID> orderStateIds,
            List<UUID> courierIds,
            UUID paymentStateId,
            UUID paymentMethodId,
            UUID managementTypeId,
            UUID storeId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
