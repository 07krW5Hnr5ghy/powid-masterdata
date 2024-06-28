package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockTransfer;
import com.proyect.masterdata.domain.StockTransferItem;
import com.proyect.masterdata.repository.StockTransferItemRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class StockTransferItemRepositoryCustomImpl implements StockTransferItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<StockTransferItem> searchForStockTransferItem(
            Long clientId,
            List<Long> stockTransferIds,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockTransferItem> criteriaQuery = criteriaBuilder.createQuery(StockTransferItem.class);

        Root<StockTransferItem> itemRoot = criteriaQuery.from(StockTransferItem.class);
        Join<StockTransferItem, StockTransfer> stockTransferItemStockTransferJoin = itemRoot.join("stockTransfer");

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                stockTransferIds,
                originWarehouseIds,
                destinationWarehouseIds,
                supplierProductIds,
                criteriaBuilder,
                itemRoot,
                stockTransferItemStockTransferJoin);
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> stockTransferItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                stockTransferItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                stockTransferItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(stockTransferItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockTransferItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                stockTransferIds,
                originWarehouseIds,
                destinationWarehouseIds,
                supplierProductIds);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            Long clientId,
            List<Long> stockTransferIds,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds,
            List<Long> supplierProductIds,
            CriteriaBuilder criteriaBuilder,
            Root<StockTransferItem> itemRoot,
            Join<StockTransferItem, StockTransfer> stockTransferItemStockTransferJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (!stockTransferIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("stockTransferId").in(stockTransferIds)));
        }

        if (!originWarehouseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(stockTransferItemStockTransferJoin.get("originWarehouseId").in(originWarehouseIds)));
        }

        if(!destinationWarehouseIds.isEmpty()){
            conditions.add(criteriaBuilder.and(stockTransferItemStockTransferJoin.get("destinationWarehouseId").in(destinationWarehouseIds)));
        }

        if(!supplierProductIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("supplierProductId").in(supplierProductIds)));
        }

        return conditions;

    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransferItem> itemRoot) {

        List<Order> stockTransferItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransferItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("stockTransferId")) {
            stockTransferItemList.add(criteriaBuilder.asc(itemRoot.get("stockTransferId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            stockTransferItemList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        return stockTransferItemList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransferItem> itemRoot) {

        List<Order> stockTransferItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransferItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("stockTransferId")) {
            stockTransferItemList.add(criteriaBuilder.desc(itemRoot.get("stockTransferId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            stockTransferItemList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        return stockTransferItemList;
    }

    private Long getOrderCount(
            Long clientId,
            List<Long> stockTransferIds,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds,
            List<Long> supplierProductIds) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockTransferItem> itemRoot = criteriaQuery.from(StockTransferItem.class);
        Join<StockTransferItem, StockTransfer> stockTransferItemStockTransferJoin = itemRoot.join("stockTransfer");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                stockTransferIds,
                originWarehouseIds,
                destinationWarehouseIds,
                supplierProductIds,
                criteriaBuilder,
                itemRoot,
                stockTransferItemStockTransferJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
