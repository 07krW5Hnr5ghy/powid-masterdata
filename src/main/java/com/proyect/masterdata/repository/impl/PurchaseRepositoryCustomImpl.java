package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.repository.PurchaseRepositoryCustom;
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
public class PurchaseRepositoryCustomImpl implements PurchaseRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Purchase> searchForPurchase(
            UUID clientId,
            List<String> serials,
            List<UUID> warehouseIds,
            List<UUID> purchaseTypeIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Purchase> criteriaQuery = criteriaBuilder.createQuery(Purchase.class);

        Root<Purchase> itemRoot = criteriaQuery.from(Purchase.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                serials,
                warehouseIds,
                purchaseTypeIds,
                status,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> purchaseList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                purchaseList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                purchaseList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(purchaseList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Purchase> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                serials,
                warehouseIds,
                purchaseTypeIds,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            List<String> serials,
            List<UUID> warehouseIds,
            List<UUID> purchaseTypeIds,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Purchase> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(!serials.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("serial").in(serials)));
        }

        if(!warehouseIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("warehouseId").in(warehouseIds)));
        }

        if(!purchaseTypeIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("purchaseTypeId").in(purchaseTypeIds)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Purchase> itemRoot) {

        List<Order> purchaseList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseTypeId")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("purchaseTypeId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseSerial")) {
            purchaseList.add(criteriaBuilder.asc(itemRoot.get("purchaseSerial")));
        }

        return purchaseList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Purchase> itemRoot) {

        List<Order> purchaseList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseTypeId")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("purchaseTypeId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseSerial")) {
            purchaseList.add(criteriaBuilder.desc(itemRoot.get("purchaseSerial")));
        }

        return purchaseList;
    }

    private Long getOrderCount(
            UUID clientId,
            List<String> serials,
            List<UUID> warehouseIds,
            List<UUID> purchaseTypeIds,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Purchase> itemRoot = criteriaQuery.from(Purchase.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                serials,
                warehouseIds,
                purchaseTypeIds,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
