package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockTransfer;
import com.proyect.masterdata.repository.StockTransferRepositoryCustom;
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
public class StockTransferRepositoryCustomImpl implements StockTransferRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<StockTransfer> searchForStockTransfer(
            Long clientId,
            List<String> serials,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockTransfer> criteriaQuery = criteriaBuilder.createQuery(StockTransfer.class);

        Root<StockTransfer> itemRoot = criteriaQuery.from(StockTransfer.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                serials,
                originWarehouseIds,
                destinationWarehouseIds,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> stockTransferList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                stockTransferList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                stockTransferList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(stockTransferList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockTransfer> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                serials,
                originWarehouseIds,
                destinationWarehouseIds);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            Long clientId,
            List<String> serials,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds,
            CriteriaBuilder criteriaBuilder,
            Root<StockTransfer> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(!serials.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("serial").in(serials)));
        }

        if (!originWarehouseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("originWarehouseId").in(originWarehouseIds)));
        }

        if (!destinationWarehouseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("destinationWarehouseId").in(destinationWarehouseIds)));
        }

        return conditions;

    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransfer> itemRoot) {

        List<Order> stockTransferList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransferList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("originWarehouseId")) {
            stockTransferList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("destinationWarehouseId")) {
            stockTransferList.add(criteriaBuilder.asc(itemRoot.get("destinationWarehouseId")));
        }

        return stockTransferList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockTransfer> itemRoot) {

        List<Order> stockTransferList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockTransferList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("originWarehouseId")) {
            stockTransferList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("destinationWarehouseId")) {
            stockTransferList.add(criteriaBuilder.desc(itemRoot.get("destinationWarehouseId")));
        }

        return stockTransferList;
    }

    private Long getOrderCount(
            Long clientId,
            List<String> serials,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockTransfer> itemRoot = criteriaQuery.from(StockTransfer.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                serials,
                originWarehouseIds,
                destinationWarehouseIds,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
