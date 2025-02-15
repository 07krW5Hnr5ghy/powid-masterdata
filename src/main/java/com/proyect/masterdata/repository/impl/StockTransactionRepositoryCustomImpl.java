package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.repository.StockTransactionRepositoryCustom;
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
public class StockTransactionRepositoryCustomImpl implements StockTransactionRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<StockTransaction> searchForStockTransaction(
            UUID clientId,
            List<String> serials,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypeIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockTransaction> criteriaQuery = criteriaBuilder.createQuery(StockTransaction.class);

        Root<StockTransaction> itemRoot = criteriaQuery.from(StockTransaction.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                serials,
                warehouseIds,
                stockTransactionTypeIds,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> stockTransactionList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                stockTransactionList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                stockTransactionList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(stockTransactionList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockTransaction> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                serials,
                warehouseIds,
                stockTransactionTypeIds
        );
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);

    }

    private List<Predicate> predicate(
            UUID clientId,
            List<String> serials,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypeIds,
            CriteriaBuilder criteriaBuilder,
            Root<StockTransaction> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (!serials.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("serial").in(serials)));
        }

        if (!warehouseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("warehouseId").in(warehouseIds)));
        }

        if (!stockTransactionTypeIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("stockTransactionTypeId").in(stockTransactionTypeIds)));
        }

        return conditions;

    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransaction> itemRoot) {

        List<Order> stockTransactionList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransactionList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("serial")) {
            stockTransactionList.add(criteriaBuilder.asc(itemRoot.get("serial")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            stockTransactionList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("stockTransactionTypeId")) {
            stockTransactionList.add(criteriaBuilder.asc(itemRoot.get("stockTransactionTypeId")));
        }

        return stockTransactionList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransaction> itemRoot) {

        List<Order> stockTransactionList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransactionList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("serial")) {
            stockTransactionList.add(criteriaBuilder.desc(itemRoot.get("serial")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            stockTransactionList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("stockTransactionTypeId")) {
            stockTransactionList.add(criteriaBuilder.desc(itemRoot.get("stockTransactionTypeId")));
        }

        return stockTransactionList;
    }

    private Long getOrderCount(
            UUID clientId,
            List<String> serials,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypeIds) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockTransaction> itemRoot = criteriaQuery.from(StockTransaction.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                serials,
                warehouseIds,
                stockTransactionTypeIds,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
