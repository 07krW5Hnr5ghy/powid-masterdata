package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockTransactionItem;
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

@Repository
public class StockTransactionItemRepositoryCustomImpl implements StockTransactionItemRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<StockTransactionItem> searchForStockTransactionItem(Long clientId, Long stockTransactionId,Long supplierProductId, String sort,
                                                                String sortColumn, Integer pageNumber, Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockTransactionItem> criteriaQuery = criteriaBuilder.createQuery(StockTransactionItem.class);

        Root<StockTransactionItem> itemRoot = criteriaQuery.from(StockTransactionItem.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(clientId, stockTransactionId, supplierProductId, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> supplierList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                supplierList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                supplierList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(supplierList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockTransactionItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(clientId, stockTransactionId, supplierProductId);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(Long clientId, Long stockTransactionId, Long supplierProductId, CriteriaBuilder criteriaBuilder,
            Root<StockTransactionItem> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (stockTransactionId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("stockTransactionId"), stockTransactionId)));
        }

        if (supplierProductId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("supplierProductId"), supplierProductId)));
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

    private Long getOrderCount(Long clientId, Long stockTransactionId, Long supplierProductId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockTransactionItem> itemRoot = criteriaQuery.from(StockTransactionItem.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(clientId, stockTransactionId, supplierProductId, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}