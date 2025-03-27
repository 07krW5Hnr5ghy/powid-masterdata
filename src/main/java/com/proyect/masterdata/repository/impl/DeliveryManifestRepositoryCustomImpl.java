package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Courier;
import com.proyect.masterdata.domain.DeliveryManifest;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.repository.DeliveryManifestRepositoryCustom;
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

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class DeliveryManifestRepositoryCustomImpl implements DeliveryManifestRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<DeliveryManifest> searchForDeliveryManifest(
            UUID clientId,
            Long manifestNumber,
            String warehouse,
            String courier,
            String courierDni,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean open) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<DeliveryManifest> criteriaQuery = criteriaBuilder.createQuery(DeliveryManifest.class);
        Root<DeliveryManifest> itemRoot = criteriaQuery.from(DeliveryManifest.class);
        Join<DeliveryManifest, Warehouse> deliveryManifestWarehouseJoin = itemRoot.join("warehouse");
        Join<DeliveryManifest, Courier> deliveryManifestCourierJoin = itemRoot.join("courier");
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                manifestNumber,
                warehouse,
                courier,
                courierDni,
                open,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                deliveryManifestWarehouseJoin,
                deliveryManifestCourierJoin
        );

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> deliveryManifestList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                deliveryManifestList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                deliveryManifestList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(deliveryManifestList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<DeliveryManifest> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);
        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getDeliveryManifestCount(
                clientId,
                manifestNumber,
                warehouse,
                courier,
                courierDni,
                open,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate
        );
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            Long manifestNumber,
            String warehouse,
            String courier,
            String courierDni,
            Boolean open,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<DeliveryManifest> itemRoot,
            Join<DeliveryManifest,Warehouse> deliveryManifestWarehouseJoin,
            Join<DeliveryManifest,Courier> deliveryManifestCourierJoin
    ) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(deliveryManifestWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }

        if(courier != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(deliveryManifestCourierJoin.get("name")),"%"+courier.toUpperCase()+"%"));
        }

        if(courierDni != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(deliveryManifestCourierJoin.get("dni"), courierDni)));
        }

        if(manifestNumber != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("manifestNumber"), "%"+manifestNumber+"%")));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if(Boolean.TRUE.equals(open)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("open"))));
        }
        if(Boolean.FALSE.equals(open)){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("open"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<DeliveryManifest> itemRoot) {

        List<Order> deliveryManifestList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            deliveryManifestList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryManifestList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryManifestList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryManifestList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryManifestList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return deliveryManifestList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<DeliveryManifest> itemRoot) {

        List<Order> deliveryManifestList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            deliveryManifestList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryManifestList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryManifestList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryManifestList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryManifestList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return deliveryManifestList;

    }

    private Long getDeliveryManifestCount(
            UUID clientId,
            Long manifestNumber,
            String warehouse,
            String courier,
            String courierDni,
            Boolean open,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate
    ){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<DeliveryManifest> itemRoot = criteriaQuery.from(DeliveryManifest.class);
        Join<DeliveryManifest, Warehouse> deliveryManifestWarehouseJoin = itemRoot.join("warehouse");
        Join<DeliveryManifest, Courier> deliveryManifestCourierJoin = itemRoot.join("courier");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));

        List<Predicate> conditions = predicate(
                clientId,
                manifestNumber,
                warehouse,
                courier,
                courierDni,
                open,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                deliveryManifestWarehouseJoin,
                deliveryManifestCourierJoin
        );

        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
