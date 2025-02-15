package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.domain.StockTransactionItem;
import com.proyect.masterdata.domain.StockTransactionType;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.repository.StockTransactionItemRepositoryCustom;
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
import java.util.UUID;

@Repository
public class StockTransactionItemRepositoryCustomImpl implements StockTransactionItemRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<StockTransactionItem> searchForStockTransactionItem(
            UUID clientId,
            List<UUID> stockTransactionIds,
            List<UUID> supplierProductIds,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypesIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockTransactionItem> criteriaQuery = criteriaBuilder.createQuery(StockTransactionItem.class);

        Root<StockTransactionItem> stockTransactionItemRoot = criteriaQuery.from(StockTransactionItem.class);
        Join<StockTransactionItem,StockTransaction> stockTransactionItemStockTransactionJoin = stockTransactionItemRoot.join("stockTransaction");
        Join<StockTransaction,Warehouse> stockTransactionWarehouseJoin = stockTransactionItemStockTransactionJoin.join("warehouse");
        Join<StockTransaction, StockTransactionType> stockTransactionStockTransactionTypeJoin = stockTransactionItemStockTransactionJoin.join("stockTransactionType");

        criteriaQuery.select(stockTransactionItemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                stockTransactionIds,
                supplierProductIds,
                warehouseIds,
                stockTransactionTypesIds,
                criteriaBuilder,
                stockTransactionItemRoot,
                stockTransactionWarehouseJoin,
                stockTransactionStockTransactionTypeJoin
        );

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> stockTransactionItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                stockTransactionItemList = listASC(sortColumn, criteriaBuilder, stockTransactionItemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                stockTransactionItemList = listDESC(sortColumn, criteriaBuilder, stockTransactionItemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(stockTransactionItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockTransactionItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                stockTransactionIds,
                supplierProductIds,
                warehouseIds,
                stockTransactionTypesIds
                );
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            List<UUID> stockTransactionIds,
            List<UUID> supplierProductIds,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypeIds,
            CriteriaBuilder criteriaBuilder,
            Root<StockTransactionItem> itemRoot,
            Join<StockTransaction,Warehouse> stockTransactionWarehouseJoin,
            Join<StockTransaction,StockTransactionType> stockTransactionStockTransactionTypeJoin
            ) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(!warehouseIds.isEmpty()){
            conditions.add(criteriaBuilder.and(stockTransactionWarehouseJoin.get("id").in(warehouseIds)));
        }

        if(!stockTransactionTypeIds.isEmpty()){
            conditions.add(criteriaBuilder.and(stockTransactionStockTransactionTypeJoin.get("id").in(stockTransactionTypeIds)));
        }

        if(!supplierProductIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("supplierProduct").get("id").in(supplierProductIds)));
        }

        if (!stockTransactionIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("stockTransactionId").in(stockTransactionIds)));
        }
        
        return conditions;

    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransactionItem> itemRoot) {

        List<Order> stockTransactionItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransactionItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("stockTransactionId")) {
            stockTransactionItemList.add(criteriaBuilder.asc(itemRoot.get("stockTransactionId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            stockTransactionItemList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        return stockTransactionItemList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransactionItem> itemRoot) {

        List<Order> stockTransactionItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransactionItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("stockTransactionId")) {
            stockTransactionItemList.add(criteriaBuilder.desc(itemRoot.get("stockTransactionId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            stockTransactionItemList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        return stockTransactionItemList;
    }

    private Long getOrderCount(
            UUID clientId,
            List<UUID> stockTransactionIds,
            List<UUID> supplierProductIds,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypeIds
    ) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockTransactionItem> stockTransactionItemRoot = criteriaQuery.from(StockTransactionItem.class);
        Join<StockTransactionItem,StockTransaction> stockTransactionItemStockTransactionJoin = stockTransactionItemRoot.join("stockTransaction");
        Join<StockTransaction,Warehouse> stockTransactionWarehouseJoin = stockTransactionItemStockTransactionJoin.join("warehouse");
        Join<StockTransaction,StockTransactionType> stockTransactionStockTransactionTypeJoin = stockTransactionItemStockTransactionJoin.join("stockTransactionType");
        criteriaQuery.select(criteriaBuilder.count(stockTransactionItemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                stockTransactionIds,
                supplierProductIds,
                warehouseIds,
                stockTransactionTypeIds,
                criteriaBuilder,
                stockTransactionItemRoot,
                stockTransactionWarehouseJoin,
                stockTransactionStockTransactionTypeJoin
        );
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}